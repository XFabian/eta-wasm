(* Implements reaching definitions

*)
open Data_structures
open Wtac

type fact = IntSetS.t VarMap.t

let flow_h l_instr (in_fact : fact) =
  let id, instr = l_instr in

  (* Kills all previous defintions of name in VarMap *)
  let add_fact var =
    match var with
    | Var name -> VarMap.add name (IntSetS.singleton id) in_fact
    | _ -> in_fact
  in
  match instr with
  | Unary { dst; _ }
  | Copy { dst; _ }
  | Load { dst; _ } ->
      add_fact dst
  | Binary { dst; _ }
  | FCall { dst = Some dst; _ } ->
      add_fact dst
  | _ -> in_fact (* No defs *)

(* Function args?? *)
module Fact = struct
  let forwards = true

  (* Map from variables to int where the int is the defintion that  *)
  type t = fact

  let insn_flow = flow_h

  let combine ds =
    let merge_fun _k l r =
      match (l, r) with
      | None, None -> None
      | None, Some s
      | Some s, None ->
          Some s
      | Some s1, Some s2 -> Some (IntSetS.union s1 s2)
    in
    List.fold_left (fun acc vm -> VarMap.merge merge_fun acc vm) VarMap.empty ds

  let compare = VarMap.compare IntSetS.compare
  let to_string = VarMap.to_string (fun _ v -> IntSetS.to_string v)
end

module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* Now build the Use- def chains
That is a data structure
Use of a variable and all defintions D of that variable that can reach that use *)

(* Maps use_id, name -> definitions that can reach it *)
type use_def_map = IntSetS.t IdVarMap.t

let get_use_def g (use_id, var) block_id instr_id ud_map =
  let reaching_defs = Graph.id_in g block_id instr_id in
  (* Globals are funky here like __stack_pinter
  They can be used before they are defined *)
  match VarMap.find_opt var reaching_defs with
  | None -> ud_map
  | Some entry ->
      Printf.printf "For variable %s used at %d: %s\n" var use_id
        (IntSetS.to_string entry);
      IdVarMap.add (use_id, var) entry ud_map

let make_use_def (g : Graph.t) =
  let reach_defs_block use_defs (block : Cfg.basic_block) =
    List.fold_left
      (fun acc_m (i, instr) ->
        let uses = Cfg.uses (i, instr) in
        (* Uses is a list *)
        List.fold_left (fun m use -> get_use_def g use block.id i m) acc_m uses)
      use_defs block.lab_instructions
  in
  List.fold_left
    (fun acc block -> reach_defs_block acc block)
    IdVarMap.empty (Graph.blocks g)

(* Inverting gives def use map *)
let invert_use_def (ud_map : use_def_map) =
  IdVarMap.fold
    (fun (use_id, var) defs du_map ->
      IntSetS.fold
        (fun def ac ->
          IdMap.update_or
            (IdVarSet.singleton (use_id, var))
            (fun v -> IdVarSet.add (use_id, var) v)
            def ac)
        defs du_map)
    ud_map IdMap.empty

(* Is not a set and does not add multiple ones for If for example *)
let extract_constant_defs lab_instructions cur_map =
  List.fold_left
    (fun acc (id, instr) ->
      match instr with
      | Copy { src = Wtac.Imm _ as imm; _ } -> IdMap.add id imm acc
      | _ -> acc)
    cur_map lab_instructions

let get_constants g =
  List.fold_left
    (fun acc (b : Cfg.basic_block) ->
      extract_constant_defs b.lab_instructions acc)
    IdMap.empty (Graph.blocks g)

let rewrite_instr i instr lookup_f =
  let subst old_var =
    match old_var with
    | Var x -> (
        match lookup_f i x with
        | Some const -> const
        | None -> old_var)
    | _ -> old_var
  in
  match instr with
  | Copy { src; dst; t } -> Copy { src = subst src; dst; t }
  | Unary { op; src; dst } -> Unary { op; src = subst src; dst }
  | Binary { op; src1; src2; dst; t } ->
      Binary { op; src1 = subst src1; src2 = subst src2; dst; t }
  | Load { src_ptr; dst } -> Load { src_ptr = subst src_ptr; dst }
  | Store { src; dst_ptr } -> Store { src = subst src; dst_ptr }
  | FCall { name; args; dst } -> FCall { name; args = List.map subst args; dst }
  | Return vs -> Return (List.map subst vs)
  | BrIf { cond; l } -> BrIf { cond = subst cond; l }
  | _ -> instr

let constant_prop g ud_map const_map =
  let lookup =
   fun i var ->
    match IdVarMap.find_opt (i, var) ud_map with
    | None -> None
    | Some defs ->
        let list_defs = IntSetS.to_list defs in
        let start_val = IdMap.find_opt (List.hd list_defs) const_map in
        List.fold_left
          (fun acc def ->
            (*Inside defs check if the def is constant  *)
            match (acc, IdMap.find_opt def const_map) with
            | _, None -> acc
            | Some (Wtac.Imm (n1, _)), Some (Wtac.Imm (n2, _)) when n1 = n2 ->
                acc
            | _, _ -> None)
          start_val list_defs
  in
  (* Fold over all uses  *)
  let new_blocks =
    List.map
      (fun (b : Cfg.basic_block) ->
        let new_instructions =
          List.map
            (fun (i, instr) -> (i, rewrite_instr i instr lookup))
            b.lab_instructions
        in
        (b.id, { b with lab_instructions = new_instructions }))
      (Graph.blocks g)
  in
  new_blocks

let analyze (cfg : Cfg.cfg) : unit =
  let init _l = VarMap.empty in
  let g = Graph.of_cfg init cfg in
  let f = Format.formatter_of_out_channel stdout in
  (* Graph.pp f g; *)
  let solved_g = Solver.solve g in
  Graph.pp f solved_g;
  Printf.printf "Use def shit";
  let ud_map = make_use_def solved_g in
  Printf.printf "%s\n"
    (IdVarMap.to_string (fun _ v -> IntSetS.to_string v) ud_map);
  (* Can be used for liveness. Each definition that is not in that list is not used
     *)
  let du_map = invert_use_def ud_map in
  Printf.printf "%s\n"
    (IdMap.to_string (fun _ v -> IdVarSet.to_string v) du_map);

  let constant_map = get_constants g in
  Printf.printf "%s\n"
    (IdMap.to_string (fun _ v -> Wtac.show_value v) constant_map);
  let new_blocks = constant_prop g ud_map constant_map in
  let new_cfg : Cfg.cfg = { label = "blub"; blocks = new_blocks } in
  let opt_g = Graph.of_cfg init new_cfg in
  Graph.pp f opt_g

(* My rewrite pass takes such a map as input and applies it to each isntruction 
    
    Get uses at each instruction.
    Look up in use def map to get definitions
    If these defintions are constant and have the same value then
      replace that use (variable) with that Constant
    *)

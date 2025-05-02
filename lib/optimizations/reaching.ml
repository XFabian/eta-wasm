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

(* TODO The constants should return a map with the use of the variable that is a 
  constant, so really a substitution map
  Similar to Copy Propagation *)
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
        (* Try to find a substitution *)
        match lookup_f i x with
        | Some new_val -> new_val
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
        let list_defs = defs |> IntSetS.to_seq |> List.of_seq in
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

(* My rewrite pass takes such a map as input and applies it to each isntruction 
    
    Get uses at each instruction.
    Look up in use def map to get definitions
    If these defintions are constant and have the same value then
      replace that use (variable) with that Constant
    *)

(* Copy Propagation
    1. Check the uses of a variable
     Use Def map.
     Check the definition 
    2. Now inbetween that value cannot be reassigned 
    x := y
    y := 5
    z := x + 1 
    It is unsafe to copy propagate x
    So check in the reaching defintions
    that all variables used in the original value are not reassigned in between!

    *)

(* TODO Globals should be make available in the entry node! *)
(* Marks which instructions need to be rewritten 
TODO: Not sure with Mem as src*)
let copy_prop_map g ud_map =
  IdVarMap.fold
    (fun (use_id, var) defs acc ->
      let defs_list = defs |> IntSetS.to_seq |> List.of_seq in
      match defs_list with
      | [ def_id ] -> (
          match Graph.lookup_instr g def_id with
          | Copy
              {
                src = (Var src_name | Mem src_name) as s;
                dst = Var dst_name;
                _;
              }
            when dst_name = var -> (
              let def_block_id = Cfg.block_id_from_id def_id in
              let use_block_id = Cfg.block_id_from_id use_id in
              let rd_at_use = Graph.id_in g use_block_id use_id in
              let rd_at_def = Graph.id_in g def_block_id def_id in
              (* Use optional type because of globals *)
              match
                ( VarMap.find_opt src_name rd_at_use,
                  VarMap.find_opt src_name rd_at_def )
              with
              | Some src_use, Some src_def ->
                  (* If the reaching definitons at the use are a subset of the 
                    defintions at defintion then it is not assigned in between
                    Otherwise the reaching defintions at use would have an entry
                    that would not exist *)
                  if IntSetS.subset src_use src_def then
                    IdMap.add use_id (var, s) acc
                  else acc
              | _ -> acc)
          | _ -> acc)
      | _ -> acc)
    ud_map IdMap.empty

let copy_prop g cp_map =
  let lookup =
   fun i var ->
    match IdMap.find_opt i cp_map with
    | None -> None
    | Some (old_var, new_val) -> if var = old_var then Some new_val else None
  in
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
  Graph.pp f opt_g;
  Printf.printf "Copy Propagation Map \n";
  let copy_map = copy_prop_map g ud_map in
  Printf.printf "%s\n"
    (IdMap.to_string (fun _ (v1, v2) -> v1 ^ "," ^ Wtac.show_value v2) copy_map);

  Printf.printf "After copy propagation";
  let solved_g' = Solver.solve opt_g in
  let copy_blocks = copy_prop solved_g' copy_map in
  let new_cfg : Cfg.cfg = { label = "blub"; blocks = copy_blocks } in
  Cfg.print_cfg "optimized.dot" new_cfg;
  let opt_g' = Graph.of_cfg init new_cfg in
  Graph.pp f opt_g'

(* So these analysis return a map with a location and a substitution map from
  string to value implemented as pairs.
  These are then used in rewrite_instr
  
  Now I would like to apply them in a loop*)

(* Statically evaluates expressions when possible
  Also does some algebraic simplifications like x + 0 = x *)

let convert_bool = Bool.to_int

let int_to_bool n =
  match n with
  | 0 -> false
  | _ -> true

let eval_bin op n1 n2 =
  match op with
  | Add -> n1 + n2
  | Sub -> n1 - n2
  | Mult -> n1 * n2
  (* This is UB if n2 is 0 we have UB so I just do anything here *)
  | Div -> if n2 = 0 then 0 else n1 / n2
  | Mod -> if n2 = 0 then 0 else n1 mod n2
  | Lt -> n1 < n2 |> convert_bool
  | Leq -> n1 <= n2 |> convert_bool
  | Gt -> n1 > n2 |> convert_bool
  | Geq -> n1 >= n2 |> convert_bool
  | Eq -> n1 = n2 |> convert_bool
  | Neq -> n1 <> n2 |> convert_bool
  | And -> (int_to_bool n1 && int_to_bool n2) |> convert_bool
  | Or -> (int_to_bool n1 || int_to_bool n2) |> convert_bool

let constant_fold_instr instr =
  match instr with
  | Binary { op; src1 = Imm (n1, _); src2 = Imm (n2, _); dst; t } ->
      let res = eval_bin op n1 n2 in
      Copy { src = Imm (res, t); dst; t }
  (* x + 0 and  0 + x -> x *)
  | Binary { op = Add; src1 = Imm (0, _); src2; dst; t } ->
      Copy { src = src2; dst; t }
  | Binary { op = Add; src1; src2 = Imm (0, _); dst; t } ->
      Copy { src = src1; dst; t }
  (* x - 0 -> x *)
  | Binary { op = Sub; src1; src2 = Imm (0, _); dst; t } ->
      Copy { src = src1; dst; t }
  (* x * 1 and 1 * x -> x *)
  | Binary { op = Mult; src1 = Imm (1, _); src2; dst; t } ->
      Copy { src = src2; dst; t }
  | Binary { op = Mult; src1; src2 = Imm (1, _); dst; t } ->
      Copy { src = src1; dst; t }
  | _ as i -> i

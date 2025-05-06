(* Computes if the set of variables that are live at each given program point *)
open Data_structures
open Dataflow
open Wtac

type fact = VarSetS.t

let extract_var = function
  | Var v -> Some v
  | _ -> None

let uses ((_, instr) : int * instruction) : string list =
  let add_opt l v =
    match extract_var v with
    | Some x -> l @ [ x ]
    | None -> l
  in
  let add_list ls vs = List.fold_left add_opt ls vs in
  match instr with
  | Return vals -> add_list [] vals
  | Unary { src; _ } -> add_opt [] src
  | Binary { src1; src2; _ } -> add_opt (add_opt [] src1) src2
  | Copy { src; _ } -> add_opt [] src
  | Load { src_ptr; _ } -> add_opt [] src_ptr
  | Store { src; _ } -> add_opt [] src
  | FCall { args; _ } -> add_list [] args
  | BrIf { cond; _ } -> add_opt [] cond
  | Br _
  | Block _
  | Loop _ ->
      [] (* uses would be handled recursively if needed *)

let def_of_instr ((_, instr) : int * instruction) : string option =
  match instr with
  | Unary { dst; _ }
  | Binary { dst; _ }
  | Copy { dst; _ }
  | Load { dst; _ }
  | FCall { dst = Some dst; _ } -> (
      match dst with
      | Var v -> Some v
      | _ -> None)
  | Store { dst_ptr : value; _ } -> (
      match dst_ptr with
      | Var v -> Some v
      | _ -> None)
  | _ -> None

module Fact = struct
  let forwards = false

  type t = fact

  (* Actually just need block but it is fine we just add the same blcok id
   *)
  let insn_flow l_instr in_fact =
    let used_vars = uses l_instr in
    (* Remove whatever is defined here *)
    let in_fact' =
      match def_of_instr l_instr with
      | Some def -> VarSetS.remove def in_fact
      | None -> in_fact
    in
    VarSetS.union in_fact' (VarSetS.of_list used_vars)

  let combine ds = List.fold_left VarSetS.union VarSetS.empty ds
  let compare = VarSetS.compare
  let to_string = VarSetS.to_string
end

module CFG = Cfg.CFG
module Graph = AsGraph (Fact) (CFG)
module Solver = Solver.Make (Fact) (Graph)

(* I think all global values need to be live in the beginning 
They essentially track side effects
*)
let analyze (cfg : Cfg.CFG.t) : Graph.t =
  (* Add globals to liveness analysis here *)
  let exit_n = CFG.get_exit cfg in
  let init l = if l = exit_n then CFG.fn_globals cfg else VarSetS.empty in
  let g = Graph.of_cfg init cfg in
  Solver.solve g

(* See this line 
IN[1006] : {a.0, b.1, tmp.7} 
             c.2 = b.1
             OUT[1006] : {a.0, tmp.7} 
Variable c.2 is not in the out set.
Since this is backwards flow we look at the out set.
Since it is not in the out Set we know it is never used later in the code.
Thus, we can remove it
*)
(* Liveness block lb :*)
let dce_block (lb : int -> fact) (b : CFG.block) =
  let new_block =
    List.fold_left
      (fun new_block (i, instr) ->
        let out_fact_i = lb i in
        Printf.printf "Fact at %d: %s\n" i (VarSetS.to_string out_fact_i);
        match def_of_instr (i, instr) with
        | Some def ->
            (* If it is not live in out set remove it  *)
            if not (VarSetS.mem def out_fact_i) then new_block
            else new_block @ [ (i, instr) ]
        | None -> new_block @ [ (i, instr) ])
      [] (CFG.l_instrs b)
  in
  Cfg.BLOCK.update_instr b new_block

(* Should probably be in its own file *)
let run_dce (lg : Graph.t) (cfg : CFG.t) : CFG.t =
  List.fold_left
    (fun cfg block_id ->
      let block = CFG.get_block cfg block_id in
      let lb = Graph.id_out lg block_id in
      let new_block = dce_block lb block in
      CFG.update_block cfg block_id new_block)
    cfg (CFG.nodes cfg)

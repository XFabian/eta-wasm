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

let analyze (cfg : Cfg.cfg) : unit =
  let init _l = VarMap.empty in
  let g = Graph.of_cfg init cfg in
  let f = Format.formatter_of_out_channel stdout in
  (* Graph.pp f g; *)
  let solved_g = Solver.solve g in
  Graph.pp f solved_g

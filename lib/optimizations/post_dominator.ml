(* Computes the Postdominators of nodes
   It is a backwards analysis
   A node n is post domniated by a node n'iff every path from n goes through n'
*)

open Data_structures

(* Maps block ids to the ids that dominate it *)
type fact = IntSetS.t

module Fact = struct
  let forwards = false

  type t = fact

  (* Actually just need block but it is fine we just add the same blcok id
   *)
  let insn_flow l_instr in_fact =
    let id, _ = l_instr in
    let block_id = Cfg.block_id_from_id id in
    IntSetS.add block_id in_fact

  let combine ds =
    let all_set = List.fold_left IntSetS.union IntSetS.empty ds in
    List.fold_left IntSetS.inter all_set ds

  let compare = IntSetS.compare
  let to_string = IntSetS.to_string
end

module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* The immediate Post dominator is the node d in pdom(n)\ n such that
   it is post dominated all other nodes in pdmon(n)\n \ d *)
let imm_pdoms g =
  let block_ids = Graph.dfa g |> IdMap.bindings |> List.map fst in
  let block_ids_no_exit = block_ids |> List.rev |> List.tl |> List.rev in
  List.fold_left
    (fun acc id ->
      Printf.printf "Computing strict pdom for %d \n" id;
      let strict_pdom = IntSetS.remove id (Graph.block_out g id) in
      Printf.printf "Strict PDom: %s\n" (IntSetS.to_string strict_pdom);
      let is_ipdom d =
        let pdom_d = Graph.block_out g d in
        let strict_pdom_no_d = IntSetS.remove d strict_pdom in
        IntSetS.for_all
          (fun d' ->
            (* let pdom_d' = Graph.block_out g d' in *)
            IntSetS.mem d' pdom_d)
          strict_pdom_no_d
      in
      match List.find_opt is_ipdom (IntSetS.elements strict_pdom) with
      | Some ipdom -> IdMap.add id ipdom acc
      | None -> failwith "No immediate Pdominator found")
    IdMap.empty block_ids_no_exit

(* In post dominator tree exit is the root! *)
let pdominator_tree ipdom_map =
  IdMap.fold
    (fun n ipdom_n tree ->
      (* Create biding with n or add it to the list of values *)
      IdMap.update_or [] (fun v -> n :: v) ipdom_n tree)
    ipdom_map IdMap.empty

let pp_pdom_tree fmt idom_tree =
  Format.fprintf fmt "digraph PostDominatorTree {@\n";
  Format.fprintf fmt "label=Post_Dominator_Tree;@\n";
  Format.fprintf fmt "labelloc=top;@\n";
  IdMap.iter
    (fun n children ->
      List.iter (fun v -> Format.fprintf fmt "%d -> %d @\n" n v) children)
    idom_tree;
  Format.fprintf fmt "}@."

let write_pdom_tree filename dom_tree =
  let dom_file = open_out filename in
  let chan = Format.formatter_of_out_channel dom_file in
  pp_pdom_tree chan dom_tree;
  close_out dom_file

let analyze (cfg : Cfg.cfg) : unit =
  let block_ids, _ = List.split cfg.blocks in
  (*  Get Id of exit node *)
  let exit_id = block_ids |> List.hd in
  let set_all_ids = IntSetS.of_list block_ids in
  let init l = if l = exit_id then IntSetS.singleton exit_id else set_all_ids in
  let g = Graph.of_cfg init cfg in
  let f = Format.formatter_of_out_channel stdout in
  (* Graph.pp f g; *)
  let solved_g = Solver.solve g in
  Graph.pp f solved_g;
  let ipdom_map = imm_pdoms solved_g in
  Printf.printf "%s\n" (IdMap.to_string (fun _ v -> Int.to_string v) ipdom_map);
  let ipdom_tree = pdominator_tree ipdom_map in
  write_pdom_tree "pdom_test.dot" ipdom_tree
(* let idom_tree = dominator_tree idom_map in *)

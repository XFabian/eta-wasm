(* Computes the dominators of a cfg
Forward and I think Must analysis
Join operation is the interection 

A node d dominates a node n in a cfg 
if 
  Every path from the entry to n passes through d
*)
open Data_structures

(* Maps block ids to the ids that dominate it *)
type fact = IntSetS.t

module Fact = struct
  let forwards = true

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

(* The root is the entry node
Each nodes parent is itrs immediate dominator
First strict dominators D = Dom[n] \ n
The immediate dominator or idom of a node n is the unique node that
strictly dominates n but does not strictly dominate any other node that
strictly dominates n. Every node reachable from the entry node has an immediate 
dominator (except the entry node)


Immediate dominator is d \in D 
And all other d' \in D -> d' \notin Dom[d]
So d does not dominate any other node
*)
(* Takes a solved dfg with domiantors *)
let immediate_doms g =
  let block_ids = Graph.dfa g |> IdMap.bindings |> List.map fst in
  let block_ids_no_entry = List.tl block_ids in
  List.fold_left
    (fun acc id ->
      let strict_dom = IntSetS.remove id (Graph.out g id) in
      Printf.printf "Strict Dom: %s\n" (IntSetS.to_string strict_dom);
      let is_idom d =
        let dom_d = Graph.out g d in
        let strict_dom_no_d = IntSetS.remove d strict_dom in
        IntSetS.for_all
          (fun d' ->
            (* let dom_d' = Graph.out g d' in *)
            IntSetS.mem d' dom_d)
          strict_dom_no_d
      in
      match List.find_opt is_idom (IntSetS.elements strict_dom) with
      | Some idom -> IdMap.add id idom acc
      | None -> failwith "No immediate dominator found")
    IdMap.empty block_ids_no_entry

let analyze (cfg : Cfg.cfg) : unit =
  let block_ids, _ = List.split cfg.blocks in
  let set_all_ids = IntSetS.of_list block_ids in
  let init l = if l = 0 then IntSetS.singleton 0 else set_all_ids in
  let g = Graph.of_cfg init cfg in
  let f = Format.formatter_of_out_channel stdout in
  (* Graph.pp f g; *)
  (* Graph.pp f g; *)
  let solved_g = Solver.solve g in
  Graph.pp f solved_g;
  let idom_map = immediate_doms solved_g in
  Printf.printf "%s\n" (IdMap.to_string (fun _ v -> Int.to_string v) idom_map);
  ()

(* Now compute the real tree from that map
  So the Idoms are the parents of the nodes Print this to dot
  Keys are the values and the values are all children of that node
  *)
(* Printf.printf "%s" (IntSetS.to_string (Graph.out solved_g 0)) *)

(* Probably related to strongly connected componetns *)
let compute_natural_loops = ()

(* TODO: Post dominators is backwards flow analysis
Useful to find the join points of an if for example
   *)

(* Also dominance frontier *)

(* Computes the dominators of a cfg
   Forward and I think Must analysis
   Join operation is the interection

   A node d dominates a node n in a cfg
   if
     Every path from the entry to n passes through d
*)
open Data_structures
open Dataflow

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

module Graph = AsGraph (Fact)(Cfg.CFG)
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
(* TODO I somewhat think that this calculation is incorrect
   But somehow it works
   The forall check will always be true I think but maybe not *)
let immediate_doms g =
  let block_ids = Graph.dfa g |> IdMap.bindings |> List.map fst in
  (* TODO: This does not remove entry. No worng. bindings sorts the list! *)
  let block_ids_no_entry = List.tl block_ids in
  List.fold_left
    (fun acc id ->
      let strict_dom = IntSetS.remove id (Graph.out g id) in
      (* Printf.printf "Strict Dom: %s\n" (IntSetS.to_string strict_dom); *)
      let is_idom d =
        let dom_d = Graph.block_out g d in
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

(* Idom map consists key = node and values are the nodes dominating the key
   We convert this to a tree
   Where the key is a node and the values are its children
   In this case the children are the nodes that are immediatly dominated by key
*)
let dominator_tree idom_map =
  IdMap.fold
    (fun n idom_n tree ->
      (* Create biding with n or add it to the list of values *)
      IdMap.update_or [] (fun v -> n :: v) idom_n tree)
    idom_map IdMap.empty

let pp_dom_tree fmt idom_tree =
  Format.fprintf fmt "digraph DominatorTree {@\n";
  Format.fprintf fmt "label=Dominator_Tree;@\n";
  Format.fprintf fmt "labelloc=top;@\n";
  IdMap.iter
    (fun n children ->
      List.iter (fun v -> Format.fprintf fmt "%d -> %d @\n" n v) children)
    idom_tree;
  Format.fprintf fmt "}@."
(* Format.pp_print_newline fmt () Flush *)

let write_dom_tree filename dom_tree =
  let dom_file = open_out filename in
  let chan = Format.formatter_of_out_channel dom_file in
  pp_dom_tree chan dom_tree;
  close_out dom_file

(* Natural loop  is defined by a back edge
   A control flow ecge B -> H where H dominates B
   Then : H Is the loop Header
   All nodes that reach B without going through H are the body of the loop
*)

let get_back_edges g =
  List.fold_left
    (fun acc (block : Cfg.CFG.block) ->
      (* Check if successor dominates this block
         Then it is a backwards edge *)
      let back_edges_n =
        List.fold_left
          (fun inner_acc dst_edge ->
            let dom_n = Graph.block_out g (Cfg.CFG.block_id block) in
            if IntSetS.mem dst_edge dom_n then ((Cfg.CFG.block_id block), dst_edge) :: inner_acc
            else inner_acc)
          [] (Cfg.CFG.succs (Graph.cfg g) (Cfg.CFG.block_id block))
      in
      back_edges_n @ acc)
    [] (Graph.blocks g)


(* Each loop consists of pairsof header and its body
These are basic blocks  *)
let compute_natural_loops g =
  let b_edges = get_back_edges g in
  let rec loop_body header body_p =
    if body_p = header then []
    else
      let block = Graph.block g body_p in
      List.fold_left
        (fun acc pred -> acc @ loop_body header pred)
        [ block ] (Cfg.BLOCK.preds block)
  in
  let loops =
    List.map
      (fun (loop_exit, loop_header) -> 
        let header_block = Graph.block g loop_header in
        header_block, loop_body loop_header loop_exit)
      b_edges
  in
  (* let open Cfg in *)
  List.iter
    (fun (header, blocks : Cfg.BLOCK.t * 'a) -> 
      Printf.printf "Header of Loop: %d\n"(Cfg.BLOCK.id header);
      Printf.printf "Body of the Loop: ";
      List.iter (fun (b : Cfg.BLOCK.t) -> Printf.printf "%d" (Cfg.BLOCK.id b)) blocks)
    loops

let analyze (cfg : Cfg.CFG.t) : unit =
  let block_ids, _ = List.split (Cfg.CFG.blocks cfg) in
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
  let idom_tree = dominator_tree idom_map in
  Printf.printf "%s\n"
    (IdMap.to_string
       (fun _ v ->
         List.fold_left (fun acc t -> acc ^ Printf.sprintf " %d," t) "" v)
       idom_tree);
  write_dom_tree "dom_test.dot" idom_tree;
  let b_edges = get_back_edges solved_g in
  List.iter (fun p -> Printf.printf "%d -> %d\n " (fst p) (snd p)) b_edges;
  compute_natural_loops solved_g
(* Printf.printf "%s" (IntSetS.to_string (Graph.out solved_g 0)) *)


(* Returns an initial dataflow graph *)
let init (cfg: Cfg.CFG.t) : Graph.t = 
  let block_ids, _ = List.split (Cfg.CFG.blocks cfg) in
  let set_all_ids = IntSetS.of_list block_ids in
  let init l = if l = 0 then IntSetS.singleton 0 else set_all_ids in
  Graph.of_cfg init cfg


let gen_debug fname (cfg: Cfg.CFG.t) = 
  let g = init cfg in 
  let solved_g = Solver.solve g in
  let idom_map = immediate_doms solved_g in
  let idom_tree = dominator_tree idom_map in
  (* let loops = compute_natural_loops solved_g in *)
  write_dom_tree fname idom_tree

(* Also dominance frontier *)

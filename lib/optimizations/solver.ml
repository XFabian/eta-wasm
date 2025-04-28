open Data_structures

(* This is a lattice *)
module type FACT = sig
  type t

  (* Join operation *)
  val combine : t list -> t
  val compare : t -> t -> int
  val to_string : t -> string
end

module type DFA_GRAPH = sig
  module NodeS : SetS

  type node = NodeS.elt
  type fact

  (* Abstract type of datflow graphs *)
  type t

  val preds : t -> node -> NodeS.t
  val succs : t -> node -> NodeS.t
  val nodes : t -> NodeS.t

  (* the flow function:
     given a graph node and input fact, compute the resulting fact on the
      output edge of the node *)
  val flow : t -> node -> fact -> fact
  val find_fact : t -> node -> fact
  val add_fact : node -> fact -> t -> t
  (* val to_string : t -> string *)
  val pp : Format.formatter -> t -> unit
end

(* The Worklist algorithm *)
module Make (Fact : FACT) (Graph : DFA_GRAPH with type fact := Fact.t) = struct
  let rec solver workset g =
    match Graph.NodeS.is_empty workset with
    | true -> g
    | false ->
        let node = Graph.NodeS.choose workset in
        let node_fact = Graph.find_fact g node in
        let pre_nodes = Graph.NodeS.elements (Graph.preds g node) in
        let in_facts = List.map (fun n -> Graph.find_fact g n) pre_nodes in
        let cmb_fact = Fact.combine in_facts in
        let out_fact = Graph.flow g node cmb_fact in
        let new_workset = Graph.NodeS.remove node workset in
        if Fact.compare node_fact out_fact <> 0 then
          let succs = Graph.succs g node in
          let g' = Graph.add_fact node out_fact g in
          let new_workset' = Graph.NodeS.union new_workset succs in
          solver new_workset' g'
        else solver new_workset g

  let solve (g : Graph.t) =
    let nodes = Graph.nodes g in
    solver nodes g
end

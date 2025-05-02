open Data_structures

module type AS_GRAPH_PARAMS = sig
  type t

  val combine : t list -> t
  val to_string : t -> string
  val forwards : bool

  (* int is the label for the basic block in the cfg *)
  val insn_flow : int * Wtac.instruction -> t -> t
end

module AsGraph (D : AS_GRAPH_PARAMS) (Cfg : Cfg.CFGT) : sig
  include Solver.DFA_GRAPH with type fact := D.t and module NodeS = IntSetS

  val of_cfg : (node -> D.t) -> Cfg.t -> t

  (* Compute the In fact for a given Node
     Our DFA only stores the Out facts *)
  val block_in : t -> node -> D.t
  val block_out : t -> node -> D.t
  val id_in : t -> node -> int -> D.t
  val id_out : t -> node -> int -> D.t
  val dfa : t -> D.t IdMap.t
  val cfg : t -> Cfg.t
  val blocks : t -> Cfg.block list
  val block : t -> int -> Cfg.block
  val lookup_instr : t -> int -> Wtac.instruction
end = struct
  module NodeS = IntSetS

  (* Initial mapping of facts to labels
     a cpmstamt f;pw om value for entry and exit *)
  type t = { cfg : Cfg.t; (* DFA has the flow values *) dfa : D.t IdMap.t }
  type node = int

  let of_cfg init cfg =
    let nodes = Cfg.blocks cfg |> List.map fst in
    let dfa =
      List.fold_left (fun acc l -> IdMap.add l (init l) acc) IdMap.empty nodes
    in
    { cfg; dfa }

  let nodes g = g.dfa |> IdMap.bindings |> List.map fst |> NodeS.of_list
  let block g n = List.assoc n (Cfg.blocks g.cfg)
  let blocks g = Cfg.blocks g.cfg |> List.split |> snd
  let exit_id g = Cfg.blocks g.cfg |> List.hd |> fst

  (* Get the instruciton with that id*)
  let lookup_instr g id =
    let block_id = id / 1000 in
    let block = block g block_id in
    let _, instr = List.find (fun (i, _) -> i = id) (Cfg.l_instrs block) in
    instr

  let preds g n =
    if D.forwards then NodeS.of_list (Cfg.preds g.cfg n)
    else NodeS.of_list (Cfg.succs g.cfg n)

  let succs g n =
    if D.forwards then NodeS.of_list (Cfg.succs g.cfg n)
    else NodeS.of_list (Cfg.preds g.cfg n)

  let add_fact n d g = { g with dfa = IdMap.add n d g.dfa }
  let out g n = IdMap.find n g.dfa
  let dfa g = g.dfa
  let cfg g = g.cfg

  (* Dataflow helpers *)
  (*
   bb is the basic block we want to flow through
   d_in is the current dataflow facts on the In edge
   Returns the resulting Out/In fact *)
  let block_flow_forwards bb (d_in : 'd) : 'd =
    (* let _, instrs = List.split bb.lab_instructions in *)
    List.fold_left (fun d i -> D.insn_flow i d) d_in (Cfg.l_instrs bb)

  let block_flow_backwards bb (d_out : 'd) : 'd =
    List.fold_right (fun i d -> D.insn_flow i d) (Cfg.l_instrs bb) d_out

  let flow_block g n f =
    (if D.forwards then block_flow_forwards else block_flow_backwards)
      (block g n) f

  (* Entry and exit nodes are special cases since they do not
     have any instructions in them
     In forwards analyis we need to ignore entry node flows
     In backwards that of exit, othersie they are not correctly updated
     For their flows. Information is not correctly propagated otherwise*)
  let flow g n f =
    if D.forwards && Int.compare n 0 = 0 then out g n
    else if (not D.forwards) && Int.compare n (exit_id g) = 0 then out g n
    else flow_block g n f

  (* Computes IN[n] for each instruction in a block given IN for the first instruction
   *)
  let in_forwards_map bb d_in : int -> 'd =
    let m, _ =
      List.fold_left
        (fun (m, f) (i, instr) ->
          let fact = D.insn_flow (i, instr) f in
          (IdMap.add i f m, fact))
        (IdMap.empty, d_in) (Cfg.l_instrs bb)
    in
    fun i -> IdMap.find i m

  (* Compupte Out[n] for each instruction given IN of the first instruction *)
  let out_forwards_map bb d_in : int -> 'd =
    let m, _ =
      List.fold_left
        (fun (m, f) (i, instr) ->
          let fact_out = D.insn_flow (i, instr) f in
          (IdMap.add i fact_out m, fact_out))
        (IdMap.empty, d_in) (Cfg.l_instrs bb)
    in
    fun i -> IdMap.find i m

  (* Compute IN[n] for each isntruction in a block, given Out of the block *)
  let in_backwards_map bb d_in : int -> 'd =
    let m, _ =
      List.fold_right
        (fun (i, instr) (m, f) ->
          let fact_in = D.insn_flow (i, instr) f in
          (IdMap.add i fact_in m, fact_in))
        (Cfg.l_instrs bb) (IdMap.empty, d_in)
    in
    fun i -> IdMap.find i m

  (* Compute Out[n] for each isntruction in a block, given Out of the block *)
  let out_backwards_map bb d_in : int -> 'd =
    let m, _ =
      List.fold_right
        (fun (i, instr) (m, f) ->
          let fact_in = D.insn_flow (i, instr) f in
          (IdMap.add i f m, fact_in))
        (Cfg.l_instrs bb) (IdMap.empty, d_in)
    in
    fun i -> IdMap.find i m

  (* Compupte the In for a node in the graph *)
  let block_in g n =
    if D.forwards then
      let preds = NodeS.elements @@ preds g n in
      let d_outs = List.map (out g) preds in
      D.combine d_outs
    else out g n

  let block_out g n =
    if D.forwards then out g n
    else
      let preds = NodeS.elements @@ preds g n in
      let d_outs = List.map (out g) preds in
      D.combine d_outs

  (* Computes the IN[n] for each isntruction in the basic block *)
  let id_in g n =
    if D.forwards then in_forwards_map (block g n) (block_in g n)
    else in_backwards_map (block g n) (block_out g n)

  let id_out g n =
    if D.forwards then out_forwards_map (block g n) (block_in g n)
    else out_backwards_map (block g n) (block_out g n)

  let printer_annot (annot : int -> string) (f : Format.formatter) (g : t) :
      unit =
    let open Format in
    let print_block f' block =
      List.iter
        (fun (i, instr) ->
          fprintf f' "IN[%d] : %s @, %a@, OUT[%d] : %s @,@," i
            (D.to_string (id_in g (Cfg.block_id block) i))
            Wtac_print.pp_instruction instr i
            (D.to_string (id_out g (Cfg.block_id block) i)))
        (Cfg.l_instrs block)
    in
    List.iter
      (fun (id, block) ->
        Format.fprintf f "%s @[<v>%a @] @." (annot id) print_block block)
      (Cfg.blocks g.cfg)

  let pp f g =
    printer_annot
      (fun l -> Format.sprintf "OUT[%d]: %s@," l (D.to_string (block_out g l)))
      f g
end

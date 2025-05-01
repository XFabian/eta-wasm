(* CFG for WTAC
   Has an explicit entry and exit nodes. Those nodes are empty
   Entry has one Successor and exit has none
*)
open Wtac

let id_counter = ref 0

let make_id () =
  let id = !id_counter in
  id_counter := id + 1;
  id

(* Inavriant Terminator is one of the branching istructions or an empty loop block *)
type basic_block = {
  id : int;
  (* label : string; *)
  (* Labelled instructoin *)
  lab_instructions : (int * Wtac.instruction) list;
  (* terminator : Wtac.instruction; *)
  mutable preds : int list;
  mutable succs : int list;
}

(* TODO Hack to get instruction ids
   id is the id of the block
   n is the number of the instruciton in the basic block *)
let get_instr_id id n = (id * 1000) + n

let new_empty_block instr =
  let id = make_id () in
  let lab_instr = List.mapi (fun i instr -> (get_instr_id id i, instr)) instr in
  { id; lab_instructions = lab_instr; preds = []; succs = [] }

let is_terminator = function
  | Wtac.Br _
  | Wtac.BrIf _
  | Wtac.Loop _
  | Wtac.Return _ ->
      true
  | _ -> false

(* Accumulate instructions until you find a terminator *)
(* Returns a basic block and the rest of instructions *)
let rec create_basic_block acc instrs : basic_block * instruction list =
  match instrs with
  | [] ->
      (* Finished we create exit block
         Cheated a little we need to add a terminator bu this needs be ignored*)
      (new_empty_block [], [])
  | i :: is when is_terminator i -> (new_empty_block (acc @ [ i ]), is)
  | i :: is -> create_basic_block (acc @ [ i ]) is

(* Records the targets of jumps *)
let resolver_map : (string, int) Hashtbl.t = Hashtbl.create 10

(* When I go int o a block I need to look into these instructions *)
let rec split_into_basic_blocks l_stack instrs =
  let split_instr (acc, blocks) (instr : instruction) =
    match instr with
    (* Blocks and loops have nested instructions *)
    | Block { body; label } ->
        (* add current instructions to body Body does not create a new basic block *)
        (* Reaminign is here for nestd blocks that have something
           after them like an if *)
        let rem_instr, body_blocks =
          split_into_basic_blocks l_stack (acc @ body)
        in
        let new_blocks = blocks @ body_blocks in
        List.iter
          (fun i -> Printf.printf "%s" (Wtac.show_instruction i))
          rem_instr;
        (* if List.length body_blocks <> 3 then failwith "Body blocks strange"
           else *)
        (* It does not parse the remaining parts of the then branch
           of the block *)
        (* The for the follwoing block is just the block id after this block
           But after else block comes the then block*)
        (* Suuuuuuuuuuuuuper ugly hack checks if label is an if_else if_end does not have l
             and could not find method to check for substring
           If it is if_end we need to jump further *)
        if String.starts_with ~prefix:label "if_en" then
          Hashtbl.replace resolver_map label (!id_counter + 1)
        else Hashtbl.replace resolver_map label !id_counter;
        (rem_instr, new_blocks)
    | Loop { body; label } ->
        let block = new_empty_block (acc @ [ Loop { label; body = [] } ]) in
        let new_blocks = blocks @ [ block ] in
        let rem_instr, loop_blocks = split_into_basic_blocks l_stack body in
        (* A loop should not have remainging instructios *)
        if List.length rem_instr <> 0 then
          failwith "Failed Loop Construction: Did not handle all isntructions"
        else
          (* The loop header is the target here the forward edge is handled by the block
             The loop header*)
          Hashtbl.replace resolver_map label (block.id + 1);
        ([], new_blocks @ loop_blocks)
    | i when is_terminator i ->
        let block = new_empty_block (acc @ [ i ]) in
        let new_blocks = blocks @ [ block ] in
        let new_acc = [] in
        (new_acc, new_blocks)
    | _ -> (acc @ [ instr ], blocks)
  in
  let rem_instr, instr_blocks = List.fold_left split_instr ([], []) instrs in
  (* List.iter (fun i -> Printf.printf "%s" (show_instruction i)) rem_instr; *)
  (rem_instr, instr_blocks)

(* CFG for a function *)
type cfg = {
  (* store basic blocks in association list, indexed by block # *)
  blocks : (int * basic_block) list;
  (* Function name for example *)
  label : string;
}

(* Emoty entry block *)
let make_entry () = new_empty_block []
let make_exit () = new_empty_block []
(* int is the basic block they target *)

type stack_label = BlockLabel of int | LoopLabel of int
(* let rec process_instrs label_stack acc parent instrs =
   match instrs with
   | [] -> (* Finished we create exit block
           Cheated a little we need to add a terminator bu this needs be ignored*)
         new_empty_block [], []
   | Loop {label; body}:: is ->
     let new_block = new_empty_block (acc @ [Loop{label; body=[]}]) in
     Stack.push (LoopLabel new_block.id) label_stack;
     let loop_blocks = split_into_basic_blocks body in

     process_instrs label_stack [] new_block.id body
     Stack.pop label_stack;
   | i::is when is_terminator i ->
     new_empty_block (acc @[i]), is
   | i::is -> process_instrs label_stack (acc @ [i]) parent is *)

(* Coming from --> to and the backward edge as well *)
let add_edge from to_ =
  from.succs <- to_.id :: from.succs;
  to_.preds <- from.id :: to_.preds

(* Very ugly hack
   We need to add the binding for exit here so if there is no return
     we automatically add it there
   However, return has no instructions so List.hd would fail
   Thus, we remove it in the actual fold call *)
let add_edges blocks =
  let edge_helper (_, b) =
    let last = snd (List.hd (List.rev b.lab_instructions)) in
    match last with
    | Br l ->
        let to_id = Hashtbl.find resolver_map l in
        let to_block = List.assoc to_id blocks in
        add_edge b to_block
    | BrIf { l; _ } ->
        (* Need to add two edges here one to the end and one to the direct successor *)
        let to_id = Hashtbl.find resolver_map l in
        let to_block = List.assoc to_id blocks in
        add_edge b to_block;
        let next_bl = List.assoc (b.id + 1) blocks in
        add_edge b next_bl
    | Loop _ ->
        (* Kept loop here for better reconstruction *)
        let next_bl = List.assoc (b.id + 1) blocks in
        add_edge b next_bl
    | _ -> ()
  in
  (* Remove exit block from list very ugly hack
     relies on the counter *)
  let blocks_no_exit = List.remove_assoc (!id_counter - 1) blocks in
  List.iter edge_helper blocks_no_exit

let cfg_helper = function
  | Wtac.Function fn ->
      let label_stack = Stack.create () in
      let entry = make_entry () in
      let rem_instr, blocks = split_into_basic_blocks label_stack fn.body in
      if List.length rem_instr <> 0 then
        failwith "Failed Cfg Construction: Did not handle all isntructions"
      else
        let exit = make_exit () in
        let chan = Format.formatter_of_out_channel stdout in
        (* let print_helper ls_i =
             List.iter (fun i -> Format.fprintf chan "%a\n" Wtac_print.pp_instruction i) ls_i
           in *)
        (* List.iteri (fun i ls -> Format.fprintf chan "Basic block %d:@," i ; print_helper ls) blocks.instructions; *)
        let print_symbols ls =
          List.iter (fun (k, v) -> Printf.fprintf stdout "%s: %d\n" k v) ls
        in
        print_symbols (Hashtbl.to_seq resolver_map |> List.of_seq);
        (* Now need to add successor edges to the graph *)
        Format.pp_print_flush chan ();
        (* TODO should clear resolver map after each function *)
        let all_blocks = (entry :: blocks) @ [ exit ] in
        print_endline "Computing edges";
        (* Entry edge since blocks can be empty link to exit *)
        add_edge entry (List.hd (blocks @ [ exit ]));
        add_edge (List.hd (List.rev (entry :: blocks))) exit;
        let cfg_blocks =
          List.fold_left (fun acc b -> (b.id, b) :: acc) [] (blocks @ [ exit ])
        in
        add_edges cfg_blocks;
        (* Dumb but works for now. Compute list again now with entry and exit *)
        let all_cfg_blocks =
          List.fold_left (fun acc b -> (b.id, b) :: acc) [] all_blocks
        in
        id_counter := 0;

        { blocks = all_cfg_blocks; label = fn.name }
  | Data _ ->
      (* Globals are ignored for now*)
      failwith "Internal Error: Data not possible after filter"

let make_cfg (Wtac.Program decls) =
  List.filter
    (fun x ->
      match x with
      | Function _ -> true
      | _ -> false)
    decls
  |> List.map cfg_helper

(* Given an instruction id returns the block that instruction resides in *)
let block_id_from_id id = id / 1000
(* let cfg_to_fn cfg =
   (* Remove main and exit *)
   let _, blocks = cfg.blocks |> List.split
   (* Start with ids as index
   We can skip basic blocks if we converted them already
     For example in if and while
       Recursively solve them and then continue afterwards *)
   let b_helper b =
     let instrs = b.instructions in
     (* Check outgoing edges *)
     match b.succs with
     | [] -> (* Only exit has that *)
         []
     | s -> (* Return and main*)
         List.assoc cfg
     | s1::s2 -> (* If and loops*)
     (* Its a loop if the predecessor of that block has a higher id in the cfg
       Then it is a later block that loops back *)
     let bb_s1 = List.assoc s1 cfg.blocks
   in
   let wtac_file = Filename.chop_extension "cfg_recover.wtac" in
   let chan = open_out wtac_file in
   let formatter = Format.formatter_of_out_channel chan in

   Wtac_print.pp_top_level formatter;
   close_out chan *)

let pp_basic_block fmt (block : basic_block) =
  let open Format in
  fprintf fmt "\"Block %d\" [label=\"%d:\\l" block.id block.id;
  List.iter
    (fun (_, instr) ->
      fprintf fmt "  ";
      Wtac_print.pp_instruction fmt instr;
      fprintf fmt "\\l")
    block.lab_instructions;
  fprintf fmt "\"];\n";
  List.iter
    (fun succ -> fprintf fmt "\"Block %d\" -> \"Block %d\";\n" block.id succ)
    block.succs

let pp_cfg fmt (blocks : (int * basic_block) list) =
  let open Format in
  fprintf fmt "digraph CFG {\n";
  fprintf fmt "  node [shape=box fontname=\"monospace\"];\n";
  List.iter (fun (_id, block) -> pp_basic_block fmt block) blocks;
  fprintf fmt "}\n"

let print_cfg f_name cfg =
  let oc = open_out f_name in
  let fmt = Format.formatter_of_out_channel oc in
  pp_cfg fmt cfg.blocks;
  close_out oc

let cfg_to_dot cfg =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "digraph cfg {\n";
  Buffer.add_string buffer
    (Printf.sprintf
       "subgraph \"test \" {]n style=\"dashed\";\n\n\
       \  color=\"black\";\n\n\
       \  label=\"Not working\";\n\n\
       \  fn_0_basic_block_0 \
        [shape=Mdiamond,style=filled,fillcolor=white,label=\"ENTRY\"];\n\n\n\n\
        \tfn_0_basic_block_1 \
        [shape=Mdiamond,style=filled,fillcolor=white,label=\"EXIT\"];\n\
       \  ");
  List.iter
    (fun (id, block) ->
      let label =
        match block.lab_instructions with
        | [] -> Printf.sprintf "Block %d" id
        | (i, x) :: xs ->
            Printf.sprintf "Block %d: \n %s" id
              (* This gets the exit block *)
              (show_instruction (snd (List.hd (List.rev ((i, x) :: xs)))))
      in
      Buffer.add_string buffer
        (Printf.sprintf "%d [shape=record,label=\"%s \"];\n" id label))
    cfg.blocks;
  (* All edges *)
  (* List.iter
     (fun edge ->
       Buffer.add_string buffer (Printf.sprintf "%d -> %d " edge.src edge.dst);
       (match edge.label with
       | None -> ()
       | Some n -> Buffer.add_string buffer (Printf.sprintf " [label=\"%s\"]" n));
       Buffer.add_string buffer "\n")
     cfg.edges; *)
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer

let write_dot_file filename contents =
  let oc = open_out filename in
  output_string oc contents;
  close_out oc

(* Define use and def for statement
   uses: Sets of variables used by s
   defs: sets of variables defined by s
   Lift to block
   in n facts that hold on entry to node n
   out n facts that hold at exit of n
   Good slides: https://groups.seas.harvard.edu/courses/cs153/2019fa/lectures/Lec20-Dataflow-analysis.pdf
*)
let extract_var = function
  | Var v -> Some v
  | _ -> None

let uses ((n, instr) : int * instruction) : (int * string) list =
  let add_opt l v =
    match extract_var v with
    | Some x -> l @ [ (n, x) ]
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

let def_of_instr (instr : instruction) : string option =
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

module type AS_GRAPH_PARAMS = sig
  type t

  val combine : t list -> t
  val to_string : t -> string
  val forwards : bool

  (* int is the label for the basic block in the cfg *)
  val insn_flow : int * instruction -> t -> t
end

open Data_structures

module AsGraph (D : AS_GRAPH_PARAMS) : sig
  include Solver.DFA_GRAPH with type fact := D.t and module NodeS = IntSetS

  val of_cfg : (node -> D.t) -> cfg -> t

  (* Compute the In fact for a given Node
     Our DFA only stores the Out facts *)
  val block_in : t -> node -> D.t
  val block_out : t -> node -> D.t
  val id_in : t -> node -> int -> D.t
  val id_out : t -> node -> int -> D.t
  val dfa : t -> D.t IdMap.t
  val blocks : t -> basic_block list
  val lookup_instr : t -> int -> instruction
end = struct
  module NodeS = IntSetS

  (* Initial mapping of facts to labels
     a cpmstamt f;pw om value for entry and exit *)
  type t = { cfg : cfg; (* DFA has the flow values *) dfa : D.t IdMap.t }
  type node = int

  let of_cfg init cfg =
    let nodes = cfg.blocks |> List.map fst in
    let dfa =
      List.fold_left (fun acc l -> IdMap.add l (init l) acc) IdMap.empty nodes
    in
    { cfg; dfa }

  let nodes g = g.dfa |> IdMap.bindings |> List.map fst |> NodeS.of_list
  let block g n = List.assoc n g.cfg.blocks
  let blocks g = g.cfg.blocks |> List.split |> snd

  (* Get the instruciton with that id*)
  let lookup_instr g id =
    let block_id = id / 1000 in
    let block = block g block_id in
    let _, instr = List.find (fun (i, _) -> i = id) block.lab_instructions in
    instr

  let preds g n =
    let block = List.assoc n g.cfg.blocks in
    if D.forwards then NodeS.of_list block.preds else NodeS.of_list block.succs

  let succs g n =
    let block = List.assoc n g.cfg.blocks in
    if D.forwards then NodeS.of_list block.succs else NodeS.of_list block.preds

  let add_fact n d g = { g with dfa = IdMap.add n d g.dfa }
  let out g n = IdMap.find n g.dfa
  let dfa g = g.dfa

  (* Dataflow helpers *)
  (*
   bb is the basic block we want to flow through
   d_in is the current dataflow facts on the In edge
   Returns the resulting Out/In fact *)
  let block_flow_forwards bb (d_in : 'd) : 'd =
    (* let _, instrs = List.split bb.lab_instructions in *)
    List.fold_left (fun d i -> D.insn_flow i d) d_in bb.lab_instructions

  let block_flow_backwards bb (d_out : 'd) : 'd =
    List.fold_right (fun i d -> D.insn_flow i d) bb.lab_instructions d_out

  let flow_block g n f =
    (if D.forwards then block_flow_forwards else block_flow_backwards)
      (block g n) f

  (* May need something for exit as well
    But entry for forwards works *)
  let flow g n f = if Int.compare n 0 = 0 then out g n else flow_block g n f

  (* Computes IN[n] for each instruction in a block given IN for the first instruction
   *)
  let in_forwards_map bb d_in : int -> 'd =
    let m, _ =
      List.fold_left
        (fun (m, f) (i, instr) ->
          let fact = D.insn_flow (i, instr) f in
          (IdMap.add i f m, fact))
        (IdMap.empty, d_in) bb.lab_instructions
    in
    fun i -> IdMap.find i m

  (* Compupte Out[n] for each instruction given IN of the first instruction *)
  let out_forwards_map bb d_in : int -> 'd =
    let m, _ =
      List.fold_left
        (fun (m, f) (i, instr) ->
          let fact_out = D.insn_flow (i, instr) f in
          (IdMap.add i fact_out m, fact_out))
        (IdMap.empty, d_in) bb.lab_instructions
    in
    fun i -> IdMap.find i m

  (* Compute IN[n] for each isntruction in a block, given Out of the block *)
  let in_backwards_map bb d_in : int -> 'd =
    let m, _ =
      List.fold_right
        (fun (i, instr) (m, f) ->
          let fact_in = D.insn_flow (i, instr) f in
          (IdMap.add i fact_in m, fact_in))
        bb.lab_instructions (IdMap.empty, d_in)
    in
    fun i -> IdMap.find i m

  (* Compute Out[n] for each isntruction in a block, given Out of the block *)
  let out_backwards_map bb d_in : int -> 'd =
    let m, _ =
      List.fold_right
        (fun (i, instr) (m, f) ->
          let fact_in = D.insn_flow (i, instr) f in
          (IdMap.add i f m, fact_in))
        bb.lab_instructions (IdMap.empty, d_in)
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
            (D.to_string (id_in g block.id i))
            Wtac_print.pp_instruction instr i
            (D.to_string (id_out g block.id i)))
        block.lab_instructions
    in
    List.iter
      (fun (id, block) ->
        Format.fprintf f "%s @[<v>%a @] @." (annot id) print_block block)
      g.cfg.blocks

  let pp f g =
    printer_annot
      (fun l -> Format.sprintf "OUT[%d]: %s@," l (D.to_string (out g l)))
      f g
end

module Fact = struct
  let forwards = true
  let insn_flow _g d = d

  (* the lattice ---------------------------------------------------------- *)
  type t = IntSetS.t

  let combine ds = List.fold_left IntSetS.union IntSetS.empty ds
  let equal = IntSetS.equal
  let compare = IntSetS.compare
  let to_string = IntSetS.to_string
end

(* instantiate the general framework ---------------------------------------- *)
module Graph = AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

let analyze (cfg : cfg) : unit =
  let init _l = IntSetS.empty in
  let g = Graph.of_cfg init cfg in
  let f = Format.formatter_of_out_channel stdout in
  Graph.pp f g

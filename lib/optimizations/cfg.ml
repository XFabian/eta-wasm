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

module type BLOCKT = sig
  type t

  val id : t -> int
  val preds : t -> int list
  val succs : t -> int list
  val lab_instructions : t -> (int * Wtac.instruction) list
  val pp : Format.formatter -> t -> unit
  val make : int -> (int * Wtac.instruction) list -> int list -> int list -> t
  val update_succs : t -> int -> unit
  val update_preds : t -> int -> unit
  val update_instr : t -> (int * Wtac.instruction) list -> t
end

module BLOCK : BLOCKT = struct
  type t = basic_block

  let id b = b.id
  let preds b = b.preds
  let succs b = b.succs
  let lab_instructions b = b.lab_instructions

  let make id lab_instructions preds succs =
    { id; lab_instructions; preds; succs }

  let update_instr b instr = { b with lab_instructions = instr }
  let update_succs b n = b.succs <- n :: b.succs
  let update_preds b n = b.preds <- n :: b.preds

  let pp fmt (block : t) =
    let open Format in
    fprintf fmt "\"Block %d\" [label=\"%d:\\l" block.id (id block);
    List.iter
      (fun (_, instr) ->
        fprintf fmt "  ";
        Wtac_print.pp_instruction fmt instr;
        fprintf fmt "\\l")
      (lab_instructions block);
    fprintf fmt "\"];\n";
    List.iter
      (fun succ ->
        fprintf fmt "\"Block %d\" -> \"Block %d\";\n" (id block) succ)
      (succs block)
end

(* TODO Hack to get instruction ids
   id is the id of the block
   n is the number of the instruciton in the basic block *)
let get_instr_id id n = (id * 1000) + n

(* Because this was added later. Blocks returns an associatoin list *)
module type CFGT = sig
  type t
  type block

  val blocks : t -> (int * block) list

  val get_block : t -> int -> block
  val fn_name : t -> string
  val fn_params : t -> string list
  val fn_type : t -> Wtac.wtac_type
  val fn_locals : t -> Wtac.obj list

  val make :
    (int * block) list ->
    string ->
    string list ->
    Wtac.wtac_type ->
    Wtac.obj list ->
    t

  val create_cfg : Wtac.program -> t list
  val succs : t -> int -> int list
  val preds : t -> int -> int list
  val l_instrs : block -> (int * Wtac.instruction) list
  val block_id : block -> int
  val print_cfg : string -> t -> unit
  val of_function : top_level -> t
  val get_entry : t -> int
  val get_exit : t -> int
end

let is_terminator = function
  | Wtac.Br _
  | Wtac.BrIf _
  | Wtac.Loop _
  | Wtac.Return _ ->
      true
  | _ -> false

(* Records the targets of jumps *)
let resolver_map : (string, int) Hashtbl.t = Hashtbl.create 10

(* Do not use block := B.t here
   This is destructive substitution
   This means it is not just substituted a tthe type level and we
   can access it
   This removes it form the signature
   So this Generated Module actaully does not satisfy the CFGT signature
   Because block is missing*)
module Make (B : BLOCKT) : CFGT with type block = B.t = struct
  type t = {
    (* store basic blocks in association list, indexed by block # *)
    blocks : (int * B.t) list;
    (* Function name for example *)
    fn_name : string;
    fn_params : string list;
    fn_type : Wtac.wtac_type;
    fn_locals : Wtac.obj list;
  }

  type block = B.t

  let make blocks fn_name fn_params fn_type fn_locals =
    { blocks; fn_name; fn_params; fn_type; fn_locals }

  let blocks cfg = cfg.blocks
  let get_block cfg n = List.assoc n cfg.blocks

  let fn_name cfg = cfg.fn_name
  let fn_params cfg = cfg.fn_params
  let fn_type cfg = cfg.fn_type
  let fn_locals cfg = cfg.fn_locals
  let succs cfg n = List.assoc n cfg.blocks |> B.succs
  let preds cfg n = List.assoc n cfg.blocks |> B.preds
  let l_instrs b = B.lab_instructions b
  let block_id b = B.id b

  let new_empty_block instr =
    let id = make_id () in
    let lab_instr =
      List.mapi (fun i instr -> (get_instr_id id i, instr)) instr
    in
    B.make id lab_instr [] []

  (* Emoty entry block *)
  let make_entry () = new_empty_block []
  let make_exit () = new_empty_block []
  (* Coming from --> to and the backward edge as well *)

  let add_edge from to_ =
    B.update_succs from (B.id to_);
    B.update_preds to_ (B.id from)

  let get_entry _cfg = 0

  (* Exit is the last block added to the cfg *)
  let get_exit cfg =
    let id, _ = List.hd cfg.blocks in
    id

  (* Very ugly hack
     We need to add the binding for exit here so if there is no return
       we automatically add it there
     However, return has no instructions so List.hd would fail
     Thus, we remove it in the actual fold call *)
  let add_edges blocks =
    let edge_helper (_, b) =
      let last = snd (List.hd (List.rev (B.lab_instructions b))) in
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
          let next_bl = List.assoc (B.id b + 1) blocks in
          add_edge b next_bl
      | Loop _ ->
          (* Kept loop here for better reconstruction *)
          let next_bl = List.assoc (B.id b + 1) blocks in
          add_edge b next_bl
      | _ -> ()
    in
    (* Remove exit block from list very ugly hack
       relies on the counter *)
    let blocks_no_exit = List.remove_assoc (!id_counter - 1) blocks in
    List.iter edge_helper blocks_no_exit

  (* Accumulate instructions until you find a terminator *)
  (* Returns a basic block and the rest of instructions
     let rec create_basic_block acc instrs : block * instruction list =
       match instrs with
       | [] ->
           (* Finished we create exit block
              Cheated a little we need to add a terminator bu this needs be ignored*)
           (new_empty_block [], [])
       | i :: is when is_terminator i -> (new_empty_block (acc @ [ i ]), is)
       | i :: is -> create_basic_block (acc @ [ i ]) is *)

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
            Hashtbl.replace resolver_map label (B.id block + 1);
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
            List.fold_left
              (fun acc b -> (B.id b, b) :: acc)
              [] (blocks @ [ exit ])
          in
          add_edges cfg_blocks;
          (* Dumb but works for now. Compute list again now with entry and exit *)
          let all_cfg_blocks =
            List.fold_left (fun acc b -> (B.id b, b) :: acc) [] all_blocks
          in
          id_counter := 0;
          make all_cfg_blocks fn.name fn.params fn.f_type fn.locals
        (* { blocks = all_cfg_blocks; label = fn.name } *)
    | Data _ ->
        (* Globals are ignored for now*)
        failwith "Internal Error: Data not possible after filter"

  (* TODO Should ony be function *)
  let of_function decl =
    match decl with
    | Wtac.Function _ as f -> cfg_helper f
    | Wtac.Data _ ->
        failwith
          "Internal Error: This function should only be \n\
          \    called with function declarations"

  let create_cfg (Wtac.Program decls) =
    List.filter
      (fun x ->
        match x with
        | Function _ -> true
        | _ -> false)
      decls
    |> List.map cfg_helper

  let pp fmt (blocks : (int * B.t) list) =
    let open Format in
    fprintf fmt "digraph CFG {\n";
    fprintf fmt "  node [shape=box fontname=\"monospace\"];\n";
    List.iter (fun (_id, block) -> B.pp fmt block) blocks;
    fprintf fmt "}\n"

  let print_cfg f_name cfg =
    let oc = open_out f_name in
    let fmt = Format.formatter_of_out_channel oc in
    pp fmt cfg.blocks;
    close_out oc

  (* Reconstruct given cfg using dominator info provided as argument *)
  (* let reconstruct (g : Solver.DFA_GRAPH.t) =
     g *)
end

(* TODO: Put block in a seperate file and make CFGT not dependent on some
   abstract block but on that specific one
   Also for ASGraph specify the block type there like in this Make functor
   Also call it CFG helper
*)
module CFG = Make (BLOCK)

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

(*
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
           match BLOCK.lab_instructions block with
           | [] -> Printf.sprintf "Block %d" id
           | (i, x) :: xs ->
               Printf.sprintf "Block %d: \n %s" id
                 (* This gets the exit block *)
                 (show_instruction (snd (List.hd (List.rev ((i, x) :: xs)))))
         in
         Buffer.add_string buffer
           (Printf.sprintf "%d [shape=record,label=\"%s \"];\n" id label))
       (CFG.blocks cfg);
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
     close_out oc *)

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

open Data_structures

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
(* module Graph = AsGraph (Fact)
   module Solver = Solver.Make (Fact) (Graph)

   let analyze (cfg : cfg) : unit =
     let init _l = IntSetS.empty in
     let g = Graph.of_cfg init cfg in
     let f = Format.formatter_of_out_channel stdout in
     Graph.pp f g *)

(* module Test = struct
     let test cfg =
       let g =Dominator.init cfg in
       g
   end *)

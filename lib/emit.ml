(* Emits WASM code from the WTAC IR *)
open Wtac

let emit_type = function 
| Types.Int -> "i64"
| Types.Bool -> "i32"
| _ -> failwith "Internal error. Other types do not have a representaiton in WASM"
let emit_bin_op out = function
  | Add -> Format.fprintf out "i64.add"
  | Sub -> Format.fprintf out "i64.sub"
  | Mult -> Format .fprintf out "i64.mul"

let show_unary_op = function
  | Negate -> "neg"
  | Not -> "not"

let emit_value out = function
(*  I would like to have for booleans the i32 type
So I need to pass in that informatin from somewhere
instructoins and values should have a type *)
  | Imm i -> Format.fprintf out "i64.const %d" i
  (* Ths should return the local index of that variable RIght now it returns the get
  which makes more sense*)
  (* TODO: Hard coded right now *)
  | Var x -> Format.fprintf out "local.get $%s" x

let emit_dst out = function
| Var name -> Format.fprintf out "local.set $%s@," name
| Imm _ -> failwith "Internal Error: Should be a variable and not an immediate"
let emit_instruction chan = function
  | Return v -> 
    Format.fprintf chan "%a@,return@,"
    emit_value v; 
  | Copy {src; dst} -> 
    (* TODO if that string is an array, then we need
      to do something differently *)
    Format.fprintf chan "%a@,%a"
    emit_value src
    emit_dst dst;
  | Binary { op; src1; src2; dst } ->
      (* Put src1 and src2 on stack *)
      Format.fprintf chan "%a@,%a@,%a@,%a"
    emit_value src1 emit_value src2
      (* Cuurent stack [src2; src1] *)
      (* Use op to pop the 2 values and put result on stack *)
      emit_bin_op  op
      (* [res] *)
      (* Copy into destination 
      Find to which local that corresponds
      *)
     emit_dst dst;
  | Unary _ -> failwith "Unary not implemented"

let emit_top_level chan = function
  | Function { name; body; locals; _ } ->
      Format.fprintf chan "@[<v 2>(func $%s@," name;

      (* Define the locals of that function *)

      (* TODO hard coded right now
      Does not differentiate between arrays and where they are stored *)
      List.iter (fun l -> Format.fprintf chan "(local $%s %s)@," l.name (emit_type l.t)) locals;
      List.iter (emit_instruction chan) body;
      Format.fprintf chan ")@]@,";
      Printf.sprintf "(export \"%s\" (func $%s))" name name
  | Data _ -> failwith "Internal Error: Data should have been handled already"

(* TODO stack pointer data_end and heap_base need to be defined and exported *)


let compiler_globals chan = 
  Format.fprintf chan "(global $__stack_pointer i32 (i32.const 0) )@,";
  Format.fprintf chan "(global $__data_end i32 (i32.const 0) )@,";
  Format.fprintf chan "(global $__heap_base i32 (i32.const 0) )@,"
let preprocess_globals chan = function 
    | Function _ as f -> Some f 
    | Data {name; t; value} ->
      Format.fprintf chan "@[(global $%s %s (%a))@]@," name (emit_type t) emit_value value;
      None

let emit wasm_file (Program decls) =
  let out_chan = open_out wasm_file in
  let formatter = Format.formatter_of_out_channel out_chan in
  (* define module memory tables etc, and the data *)
  Format.fprintf formatter "@[<v 2>(module@,";
  (* define all globals *)
  compiler_globals formatter;
  let funcs = List.filter_map (preprocess_globals formatter) decls in

  let export_strings = List.map (emit_top_level formatter) funcs in
  (* Last export all functions *)
  List.iter
    (fun exp_func -> Format.fprintf formatter "%s@," exp_func)
    export_strings;
  Format.fprintf formatter ")@]@.";
  close_out out_chan

(* This is valid (local i32 i32 i32 i32 i32) *)

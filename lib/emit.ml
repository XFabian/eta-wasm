(* Emits WASM code from the WTAC IR *)
open Wtac

let emit_bin_op out = function
  | Add -> Printf.fprintf out "i64.add\n"
  | Sub -> Printf.fprintf out "i64.sub\n"
  | Mult -> Printf.fprintf out "i64.mul\n"

let show_unary_op = function
  | Negate -> "neg"
  | Not -> "not"

let emit_value out = function
  | Imm i -> Printf.fprintf out "i64.const %d\n" i
  (* Ths should return the local index of that variable RIght now it returns the get
  which makes more sense*)
  (* TODO: Hard coded right now *)
  | Var _ -> Printf.fprintf out "local.get 0 \n"

let emit_instruction chan = function
  | Return _ -> failwith "Return not implemented"
  | Binary { op; src1; src2; dst } -> (
      (* Put src1 and src2 on stack *)
      emit_value chan src1;
      emit_value chan src2;
      (* Cuurent stack [src2; src1] *)
      (* Use op to pop the 2 values and put result on stack *)
      emit_bin_op chan op;
      (* [res] *)
      (* Copy into destination 
      Find to which local that corresponds
      TODO Have a func_info record that recors all these additonal informations*)
      match dst with
      | Var name -> Printf.fprintf chan "local.set $%s\n" name
      | Imm _ ->
          failwith "Internal Error: Should be a variable and not an immediate")
  | Unary _ -> failwith "Unary not implemented"

let emit_top_level chan = function
  | Function { name; body; locals; _ } ->
      Printf.fprintf chan "(func $%s\n" name;

      (* Define the locals of that function *)

      (* TODO hard coded right now *)
      List.iter (fun l -> Printf.fprintf chan "(local $%s i64)\n" l) locals;
      List.iter (emit_instruction chan) body;
      Printf.fprintf chan ")\n";
      Printf.sprintf "(export \"%s\" (func $%s))" name name
  | Data _ -> failwith "Internal Error: Not implemented right now"

(* TODO stack pointer data_end and heap_base need to be defined and exported *)
let emit (Program decls) =
  let out_chan = open_out "test.wat" in
  (* define module memory tables etc, and the data *)
  Printf.fprintf out_chan "(module\n";
  let export_strings = List.map (emit_top_level out_chan) decls in
  (* Last export all functions *)
  List.iter
    (fun exp_func -> Printf.fprintf out_chan "%s\n" exp_func)
    export_strings;
  Printf.fprintf out_chan ")";
  close_out out_chan

(* This is valid (local i32 i32 i32 i32 i32) *)

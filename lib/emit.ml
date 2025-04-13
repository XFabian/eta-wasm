(* Emits WASM code from the WTAC IR *)
open Wtac

let emit_type = function
  | Types.Int -> "i64"
  | Types.Bool -> "i32"
  (* TODO : Not sure if right. But array is a pointer  *)
  | Types.Array _ -> "i32"
  | _ ->
      failwith
        "Internal error. Other types do not have a representaiton in WASM"

(* Equal and not equal depend on type of operands 
Similar for add*)
let emit_bin_op out = function
  | Add -> Format.fprintf out "i64.add"
  | Sub -> Format.fprintf out "i64.sub"
  | Mult -> Format.fprintf out "i64.mul"
  | And -> Format.fprintf out "i32.and"
  | Or -> Format.fprintf out "i32.or"
  | Lt -> Format.fprintf out "i64.lt_s"
  | Leq -> Format.fprintf out "i64.le_s" 
  | Gt -> Format.fprintf out "i64.gt_s" 
  | Geq -> Format.fprintf out "i64.ge_s"
  | _ -> failwith "Equal and not equal Not impemented"

let show_unary_op = function
  | Negate -> "i64.sub"
  | Not -> "i32.eqz"

let emit_value out = function
  (* I would like to have for booleans the i32 type
     So I need to pass in that informatin from somewhere
     instructoins and values should have a type *)
  | Imm i -> Format.fprintf out "i64.const %d" i
  (* Ths should return the local index of that variable RIght now it returns the get
     which makes more sense*)
  (* TODO: Hard coded right now *)
  | Var x -> (
      match Symbols.get_opt x with
      | Some { attr = Symbols.Local; _ } -> Format.fprintf out "local.get $%s" x
      | Some { attr = Symbols.Global; _ } ->
          Format.fprintf out "global.get $%s" x
      | _ -> failwith "Internal Error: Variable not found during Emit")
  | Mem name -> 
    (match Symbols.get_opt name with
    | Some {t=Types.Array{elem_type}; _} -> Format.fprintf out "%s.load"(emit_type elem_type)
    | _ -> failwith "Internal Error: Memory should have array type"
    )

let emit_dst out = function
  | Var name -> (
      match Symbols.get_opt name with
      | Some { attr = Symbols.Local; _ } ->
          Format.fprintf out "local.set $%s" name
      | Some { attr = Symbols.Global; _ } ->
          Format.fprintf out "global.set $%s" name
      | _ -> failwith "Internal Error: Variable not found during Emit")
  | Mem name -> 
    (* TODO to emit a certain store, we need the type info here
    We need to know if we store a bool, int or a ptr *)
      (match Symbols.get_opt name with
    | Some {t=Types.Array{elem_type}; _} -> Format.fprintf out "%s.store"(emit_type elem_type)
    | _ -> failwith "Internal Error: Memory should have array type"
    )
  | Imm _ ->
      failwith "Internal Error: Should be a variable and not an immediate"

let empty ppf () = Format.fprintf ppf ""

let rec emit_instruction chan = function
  | Return [ v ] -> Format.fprintf chan "%a@,return@," emit_value v
  | Return _ -> failwith "Handle only single return value right now"
  | Copy { src; dst=Mem name } ->
    (* We store at Mem *)
    Format.fprintf chan "local.get %s@,%a@,%a@," name emit_value src emit_dst (Mem name)
  | Copy { src; dst } ->
      (* TODO if that string is an array, then we need
         to do something differently *)
      Format.fprintf chan "%a@,%a@," emit_value src emit_dst dst
  
  | Unary { op = Negate; src; dst } ->
      Format.fprintf chan "i64.const 0@,%a@,%s@,%a@," emit_value src
        (show_unary_op Negate) emit_dst dst
  | Unary { op = Not; src; dst } ->
      Format.fprintf chan "%a@,%s@,%a@," emit_value src (show_unary_op Not)
        emit_dst dst
  | Binary { op; src1; src2; dst } ->
      (* Put src1 and src2 on stack *)
      Format.fprintf chan "%a@,%a@,%a@,%a@," emit_value src1 emit_value src2
        (* Cuurent stack [src2; src1] *)
        (* Use op to pop the 2 values and put result on stack *)
        emit_bin_op op
        (* [res] *)
        (* Copy into destination
           Find to which local that corresponds
        *)
        emit_dst dst
  | Br l -> Format.fprintf chan "br $%a@," Format.pp_print_string l
  | BrIf { cond; l } ->
      Format.fprintf chan "%a@," emit_value cond;
      Format.fprintf chan "br_if $%a@," Format.pp_print_string l
  | Block { label; body } ->
      Format.fprintf chan "@[<v 2>(block $%a@,%a@])@," Format.pp_print_string
        label
        (Format.pp_print_list ~pp_sep:empty emit_instruction)
        body
  | Loop { label; body } ->
      Format.fprintf chan "@[<v 2>(loop $%a@,%a@])@," Format.pp_print_string
        label
        (Format.pp_print_list ~pp_sep:empty emit_instruction)
        body
  | FCall { name; args; dst } ->
      Format.pp_print_list emit_value chan args;
      (* TODO: For multi value not working *)
      (match dst with 
      | Some d ->
      Format.fprintf chan "@,call $%s@,%a@," name emit_dst d
      | None -> Format.fprintf chan "@,call $%s@," name)
  | _ -> failwith "Load and store not implemnted"

let emit_top_level chan = function
  | Function
      { name; body; locals; params; f_type = FunType { param_types; ret_type } }
    ->
      let combined_pt = List.combine params param_types in
      let pp_v fmt (param, ty) =
        Format.fprintf fmt "(param $%s %s) " param (emit_type ty)
      in
      Format.fprintf chan "@[<v 2>(func $%s " name;
      Format.pp_print_list pp_v chan combined_pt;
      (match ret_type with
      | [ r_t ] -> Format.fprintf chan "(result %s)@," (emit_type r_t)
      | [] -> Format.fprintf chan "@,"
      | _ -> failwith "Multiple return types are not supported right now");

      (* TODO check that out in Rust by returning a typle *)
      (* Define the locals of that function *)

      (* TODO hard coded right now
         Does not differentiate between arrays and where they are stored *)
      List.iter
        (fun l -> Format.fprintf chan "(local $%s %s)@," l.name (emit_type l.t))
        locals;
      List.iter (emit_instruction chan) body;
      Format.fprintf chan ")@]@,";
      Printf.sprintf "(export \"%s\" (func $%s))" name name
  | Data _ -> failwith "Internal Error: Data should have been handled already"
  | _ -> failwith "Internal Error: Function has non Function type"
(* TODO stack pointer data_end and heap_base need to be defined and exported *)

let compiler_globals chan =
  Format.fprintf chan "(global $__stack_pointer i32 (i32.const 0) )@,";
  Format.fprintf chan "(global $__data_end i32 (i32.const 0) )@,";
  Format.fprintf chan "(global $__heap_base i32 (i32.const 0) )@,"

let preprocess_globals chan = function
  | Function _ as f -> Some f
  | Data { name; t; value } ->
      Format.fprintf chan "@[(global $%s %s (%a))@]@," name (emit_type t)
        emit_value value;
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

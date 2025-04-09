open Wtac

let comma_sep out () = Format.fprintf out ", "

let pp_un_op out = function
  | Negate -> Format.pp_print_string out "-"
  | Not -> Format.pp_print_string out "!"

let pp_bin_op out = function
  | Add -> Format.pp_print_string out "+"
  | Sub -> Format.pp_print_string out "-"
  | Mult -> Format.pp_print_string out "*"
  | And -> Format.pp_print_string out "&"
  | Or -> Format.pp_print_string out "|"

let pp_value out = function
  | Imm i -> Format.pp_print_int out i
  | Var s -> Format.pp_print_string out s

let rec pp_instruction out = function
  | Return v -> Format.fprintf out "Return(%a)" pp_value v
  | Unary { op; src; dst } ->
      Format.fprintf out "%a = %a%a" pp_value dst pp_un_op op pp_value src
  | Binary { op; src1; src2; dst } ->
      Format.fprintf out "%a = %a %a %a" pp_value dst pp_value src1 pp_bin_op op
        pp_value src2
  | Copy { src; dst } -> Format.fprintf out "%a = %a" pp_value dst pp_value src
  | Block { label; body } ->
      Format.fprintf out "@[<v 2>block %a:@, %a@]" Format.pp_print_string label
        (Format.pp_print_list pp_instruction)
        body
  | Br l -> Format.fprintf out "br %s" l
  | BrIf { cond; l } -> Format.fprintf out "br %a, %s" pp_value cond l

let pp_function_definition out name params _f_type body =
  (* Format.pp_set_margin out 40; *)
  Format.pp_open_vbox out 0;
  Format.fprintf out "%s(%a):" name
    Format.(pp_print_list ~pp_sep:comma_sep pp_print_string)
    params;
  Format.pp_print_break out 0 4;
  Format.pp_open_vbox out 0;
  Format.pp_print_list pp_instruction out body;
  Format.pp_close_box out ();
  Format.pp_close_box out ()

let pp_top_level out = function
  | Function { name; params; f_type; body; _ } ->
      pp_function_definition out name params f_type body
  | Data { name; t; value } ->
      Format.pp_open_vbox out 0;
      Format.fprintf out "global %s : %a = %a" name Types.pp t pp_value value;
      Format.pp_close_box out ()

let pp_program out (Program decls) =
  Format.pp_open_vbox out 0;
  Format.pp_print_list
    ~pp_sep:(fun out () ->
      (* print _two_ newlines b/t functions *)
      Format.pp_print_cut out ();
      Format.pp_print_cut out ())
    pp_top_level out decls;
  Format.pp_close_box out ();
  Format.pp_print_newline out () (* flush *)

let print_wtac src_filename tacky_ast =
  let wtac_file = Filename.chop_extension src_filename ^ ".debug.wtac" in
  let chan = open_out wtac_file in
  let formatter = Format.formatter_of_out_channel chan in
  pp_program formatter tacky_ast;
  close_out chan

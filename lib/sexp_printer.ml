(* An interface for usage by students to help them write the SExpr *)

(* Finish this when hte grammar is full *)
(* No just let them define it using Sexp.Atom and Sexp.List
We then use already existing printers

*)

module S = struct
  include Sexplib.Sexp
end

(* Functoins differ from Cornell, we put parameters and return types in one list
e.g. (() (bool int)) instaed of () (bool int) 
That is actually not working parameters use their args as well*)
(* Fun Declarations are unclear parameters alone or with their tpye?
Accordig to grammar with type *)
let rec to_sexp_type = function 
| Types.Int -> S.Atom "int"
| Types.Bool -> S.Atom "bool"
| Types.Array {elem_type} -> S.List [S.Atom "[]"; S.List [to_sexp_type elem_type]]
| Types.FunType _ ->
  failwith "Error Sexp: Function Type should not be called directly"
  (* This shoud not be called *)
  (* let s_param = Sexplib.Std.sexp_of_list to_sexp_type param_types in 
  let s_ret = Sexplib.Std.sexp_of_option to_sexp_type ret_type in 
  S.List [s_param;s_ret] *)
let to_sexp_binop = function 
| Ast.Add -> S.Atom "+"
| Ast.Sub -> S.Atom "-"
| Ast.Mult -> S.Atom "*" 
| Ast.And -> S.Atom "&"
| Ast.Or-> S.Atom "|"

let to_sexp_unop = function 
| Ast.Not -> S.Atom "!"
| Ast.Neg -> S.Atom "-"

let rec to_sexp_expr = function 
| Ast.Var x -> S.Atom x
| Ast.Int i -> Sexplib.Std.sexp_of_int i 
| Ast.Bool b -> Sexplib.Std.sexp_of_bool b
| Ast.Unary (op, e) -> 
  S.List [to_sexp_unop op; to_sexp_expr e]
| Ast.Binary (op, e1, e2) ->
  S.List [to_sexp_binop op; to_sexp_expr e1; to_sexp_expr e2]
| Ast.FunCall {name; args} -> 
  let s_args = List.map to_sexp_expr args in
    S.List (S.Atom name::s_args)

let rec to_sexp_stmt = function 
| Ast.Return e -> S.List [S.Atom "return"; to_sexp_expr e]
| Compound bl -> to_sexp_block bl
| Assign {lhs; rhs} -> 
  failwith ""
| Ast.If _ -> failwith "" 
| _ -> failwith ""


and to_sexp_block (Ast.Block bl) = 
  Sexplib.Std.sexp_of_list to_sexp_stmt bl

let to_sexp_top_level = function 
  | Ast.FunDecl {name; params; f_type; body} -> 
    let p_types, ret_type = match f_type with
    | FunType {param_types; ret_type} -> List.combine params param_types, ret_type
    | _ -> failwith "Function with non funciton type!"
    in
    let to_sexp_param (p,t) = 
      S.List [S.Atom p; to_sexp_type t]
    in 
    let s_p_types = Sexplib.Std.sexp_of_list to_sexp_param p_types in
    S.List [S.Atom name; s_p_types; Sexplib.Std.sexp_of_option to_sexp_type ret_type]
  | Global {name; init; var_type} ->
    let s_init = Sexplib.Std.sexp_of_option to_sexp_expr init in
    S.List [S.Atom "global:";S.Atom name; to_sexp_type var_type; s_init]


let sexp_of_prog (Ast.Program decls) = 
  let s_decls = List.map to_sexp_top_level decls in 
  S.List s_decls
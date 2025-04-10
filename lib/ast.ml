open Sexplib.Std

(* Derives S-Expressions for base types *)
type bin_op = Add | Sub | Mult | And | Or [@@deriving show, sexp]
type unary_op = Not | Neg [@@deriving show, sexp]

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Unary of unary_op * expr
  | Binary of bin_op * expr * expr
  | FunCall of { name : string; args : expr list }
[@@deriving show, sexp]

type var_declaration = { name : string; init : expr option; var_type : Types.t }
[@@deriving show, sexp]

type stmt =
  | LocalDecl of var_declaration
  | Return of expr
  | Assign of { lhs : expr; rhs : expr }
  | Compound of block
  | If of { guard : expr; then_br : stmt; else_br : stmt option }
  | While of { guard : expr; body : stmt }
  | Procedure of { name : string; args : expr list }
[@@deriving show, sexp]

and block = Block of stmt list [@@deriving show, sexp]

type func_declaration = {
  name : string;
  params : string list;
  f_type : Types.t;
  body : block;
}
[@@deriving show, sexp]

type declaration = Global of var_declaration | FunDecl of func_declaration
[@@deriving show, sexp]

type prog = Program of declaration list [@@deriving show, sexp]

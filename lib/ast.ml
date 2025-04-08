type bin_op = Add | Sub | Mult [@@deriving show]

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Binary of bin_op * expr * expr
  | FunCall of {name:string; args : expr list}
[@@deriving show]

type var_declaration = { name : string; init : expr option; var_type : Types.t }
[@@deriving show]

type stmt =
  | LocalDecl of var_declaration
  | Return of expr
  | Assign of { lhs : expr; rhs : expr }
  | Compound of block
  | If of { guard : expr; then_br : stmt; else_br : stmt option }
  | While of { guard : expr; body : stmt }
  | Procedure of { name : string; args : expr list }
[@@deriving show]

and block = Block of stmt list [@@deriving show]

type func_declaration = {
  name : string;
  params : string list;
  f_type : Types.t;
  body : block;
}
[@@deriving show]

type declaration = Global of var_declaration | FunDecl of func_declaration
[@@deriving show]

type prog = Program of declaration list [@@deriving show]

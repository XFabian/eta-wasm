type bin_op = Add | Sub | Mult [@@deriving show]
type expr = Int of int | Binary of bin_op * expr * expr [@@deriving show]

type var_declaration = { name : string; init : expr option; var_type : Types.t }
[@@deriving show]

type stmt = Expression of expr
and block_item = S of stmt | LocalDecl of var_declaration
and block = Block of block_item list

type func_declaration = {
  name : string;
  params : string list;
  f_type : Types.t;
  body : expr;
}
[@@deriving show]

type declaration = Global of var_declaration | FunDecl of func_declaration
[@@deriving show]

type prog = Program of declaration list [@@deriving show]

type bin_op = Add | Sub | Mult [@@deriving show]
type expr = Int of int | Binary of bin_op * expr * expr [@@deriving show]

type func_declaration = {
  name : string;
  params : string list;
  f_type : Types.t;
  body : expr;
}
[@@deriving show]

type declaration = FunDecl of func_declaration [@@deriving show]
type prog = Program of declaration list [@@deriving show]

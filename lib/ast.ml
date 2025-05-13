open Sexplib.Std

(* Derives S-Expressions for base types *)
type bin_op =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | And
  | Or
  | Lt
  | Leq
  | Gt
  | Geq
  | Eq
  | Neq
[@@deriving show, sexp]

type unary_op = Not | Neg [@@deriving show, sexp]

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  (* Array literals like "Hello" and {1,2,3,4} *)
  | ArrayLit of expr list
  | Unary of unary_op * expr
  | Binary of bin_op * expr * expr
  | FunCall of { name : string; args : expr list }
  (* a[1] for example or a[2][3] Subscript(a, Subscript 2 ,3) *)
  | Subscript of expr * expr
  (* p.x for example or p.y.x *)
  | Dot of expr * string
[@@deriving show, sexp]

(* Need to have array_declaration here, since that keeps track of the dimension
which I need
*)
type var_declaration = { name : string; init : expr option; var_type : Types.t }
[@@deriving show, sexp]

type assign_target = V of var_declaration | E of expr [@@deriving show, sexp]

type stmt =
  | LocalDecl of var_declaration
  (* We need to keep track of the dimension like c : int[2][] will have Some 2: None *)
  | ArrayDecl of {
      name : string;
      a_type : Types.t;
      dims : expr option list;
      init : expr option;
    }
  | Return of expr list
  | Assign of { lhs : assign_target list; rhs : expr list }
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
(* Each element in a record takes 64 bytes

*)
[@@deriving show, sexp]

type record_declaration = { tag : string; members : member_declaration list }
[@@deriving show, sexp]

and member_declaration = { name : string; t : Types.t } [@@deriving show, sexp]

type declaration =
  | Global of var_declaration
  | FunDecl of func_declaration
  | RecordDecl of record_declaration
[@@deriving show, sexp]

type prog = Program of declaration list [@@deriving show, sexp]

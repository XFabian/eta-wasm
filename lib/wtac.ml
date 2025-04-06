(* TAC IR for our language *)
type unary_op = Negate | Not
type binary_op = Add | Sub | Mult
(* | Div
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq *)

type value = Imm of int | Var of string

type instruction =
  | Return of value
  | Unary of { op : unary_op; src : value; dst : value }
  | Binary of { op : binary_op; src1 : value; src2 : value; dst : value }

(* DATA is used for globals *)
type top_level =
  | Function of {
      name : string;
      body : instruction list;
      params : string list;
      f_type : Types.t;
    }
  | Data of { name : string; t : Types.t }

type program = Program of top_level list

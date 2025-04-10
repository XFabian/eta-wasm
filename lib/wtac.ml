(* TAC IR for our language
   Has structured control flow
*)
type unary_op = Negate | Not

type binary_op =
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

(* Maybe record that information somewhere. So during one function we can record all
   created variable
   then we reset it in the end
   Very ugly rioght now
*)
(* Maybe it should be a map from string to type *)
type obj = { name : string; t : Types.t }


type value = Imm of int | Var of string

type instruction =
  | Return of value
  | Unary of { op : unary_op; src : value; dst : value }
  | Binary of { op : binary_op; src1 : value; src2 : value; dst : value }
  | Copy of { src : value; dst : value }
  (* Sometimes called Move needed for assignments and declarations*)
  (* Blocks as in WASM
     a br instruction exits the innermost block.
     a br_if instruction
  *)
  | Block of { label : string; body : instruction list }
  | Br of string
  | BrIf of { cond : value; l : string }

(* DATA is used for globals *)
type top_level =
  | Function of {
      name : string;
      body : instruction list;
      params : string list;
      f_type : Types.t;
      locals : obj list;
    }
    (* After semantic nalysis we know that init has to be a literal const!
       Arrays are not initialzed in global *)
  | Data of { name : string; t : Types.t; value : value }

type program = Program of top_level list

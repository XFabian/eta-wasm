(* TAC IR for our language 
After evaluation this does not quite work easily.
WASM has structured control flow which we lose here.
We could recover this as is done in Beyond Relooper, however, the algorithm 
is complicated and tricky.

That is why the next iteration of the IR is more targeted to WASM
It will not include unstructured control Flow

*)
type unary_op = Negate | Not
type binary_op = Add | Sub | Mult | And | Or

(* Maybe record that information somewhere. So during one function we can record all
   created variable
   then we reset it in the end
   Very ugly rioght now
*)
(* Maybe it should be a map from string to type *)
type obj = { name : string; t : Types.t }

(* | Div
   | Mod

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
  | Copy of { src : value; dst : value }
  (* Sometimes called Move needed for assignments and declarations*)
  (* Some discussion if we want to have name(x) as a value an dlet
     Jump andCJump take values instead.
  *)
  | Label of string
  | Jump of string
  | CJump of { cond : value; l1 : string; l2 : string }

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

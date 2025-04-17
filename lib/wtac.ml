(* TAC IR for our language
   Has structured control flow
*)

type wtac_type = | I32 | I64 | FunType of {param_type : wtac_type list; ret_type: wtac_type option}
[@@deriving show]
type unary_op = Negate | Not
[@@deriving show]

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
  [@@deriving show]

(* Maybe record that information somewhere. So during one function we can record all
   created variable
   then we reset it in the end
   Very ugly rioght now
*)
(* Maybe it should be a map from string to type *)
type obj = { name : string; t : wtac_type }
[@@deriving show]

(* Mem should be a value as well for arrays for example
Copy with a mem *)
(* We tpe immediates so we know which size to emit *)
type value = Imm of int * wtac_type | Var of string | Mem of string
[@@deriving show]

(* Mem is used to represent the memory locatoin of something.
So we sometimes convert to it. We use it to differentiate in Copy if
  we just assign or we do a load or store
Copy(Mem()), 2) stores at Mem where Mem is dst
Copy (x, Mem) loads a value from Mem and stores it in x
Maybe at an offset to Mem*)
type instruction =
  | Return of value list
  | Unary of { op : unary_op; src : value; dst : value }
  | Binary of { op : binary_op; src1 : value; src2 : value; dst : value; t: wtac_type }
  | Copy of { src : value; dst : value; t : wtac_type }
  (* Sometimes called Move needed for assignments and declarations*)
  (* Blocks as in WASM
     a br instruction exits the innermost block.
     a br_if instruction
  *)
  
  | Load of {src_ptr: value; dst: value}
  | Store of {src: value; dst_ptr: value}
  | FCall of { name : string; args : value list; dst : value option}
  | Block of { label : string; body : instruction list }
  | Loop of {label : string; body : instruction list}
  | Br of string
  | BrIf of { cond : value; l : string }
  [@@deriving show]

(* DATA is used for globals *)
type top_level =
  | Function of {
      name : string;
      body : instruction list;
      params : string list;
      f_type : wtac_type;
      locals : obj list;
    }
    (* After semantic nalysis we know that init has to be a literal const!
       Arrays are not initialzed in global *)
  | Data of { name : string; t : Types.t; value : value }
  [@@deriving show]

type program = Program of top_level list
[@@deriving show]

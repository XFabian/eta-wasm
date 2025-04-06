type t =
| Int 
| Bool 
| Array of {elem_type : t; size : int}
| FunType of {param_types : t list; ret_type : t}
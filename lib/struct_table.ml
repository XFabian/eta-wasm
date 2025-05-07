(* Keeps track of structs for typechecking and maybe later on *)

module StringMap = Map.Make (String)

(* TODO If I want to keep track of offsets do it here *)
type member_entry = { member_t : Types.t }
type struct_entry = { members : member_entry StringMap.t }

let struct_table : (string, struct_entry) Hashtbl.t = Hashtbl.create 20
let add_struct tag struct_def = Hashtbl.replace struct_table tag struct_def
let mem tag = Hashtbl.mem struct_table tag
let find tag = Hashtbl.find struct_table tag

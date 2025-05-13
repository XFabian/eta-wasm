(* Keeps track of structs for typechecking and maybe later on *)

module StringMap = Map.Make (String)

(* Store positive offsets. Note that Stack grows downwards if stored on stack *)
type member_entry = { member_t : Types.t; offset : int }
type struct_entry = { members : member_entry StringMap.t }

let struct_table : (string, struct_entry) Hashtbl.t = Hashtbl.create 20
let add_struct tag struct_def = Hashtbl.replace struct_table tag struct_def
let mem tag = Hashtbl.mem struct_table tag
let find tag = Hashtbl.find struct_table tag

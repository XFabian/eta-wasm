(* IMPORTANT This is currently not used and not needed
 *)
(* TODO: Probably do not need globals since I track them using the DATA type *)
type attrs = Fun of { bytes_required : int } | Global | Local
type entry = { t : Types.t; attr : attrs }

(* Since every declaration has a unique name across functoins
we can just use a single map.
Otherwise we would need a map per function.
So another level of indirection. That would work as well *)
let symbol_table : (string, entry) Hashtbl.t = Hashtbl.create 16

(* Apply function f to entry in symbol table at key k *)
let modify k f =
  let old_entry = Hashtbl.find symbol_table k in
  Hashtbl.replace symbol_table k (f old_entry)

let add_local_var name ty =
  Hashtbl.replace symbol_table name { t = ty; attr = Local }

let add_global_var name ty =
  Hashtbl.replace symbol_table name { t = ty; attr = Global }

(* Right now does the smae thing but maybe keep stack frame size *)
(* let add_fun name ty = 
  Hashtbl.replace symbol_table name { t = ty } *)
let get name = Hashtbl.find symbol_table name
let get_opt name = Hashtbl.find_opt symbol_table name
let bindings () = Hashtbl.to_seq symbol_table |> List.of_seq

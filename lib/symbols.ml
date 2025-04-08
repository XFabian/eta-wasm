(* IMPORTANT This is currently not used and not needed
 *)
(* TODO: Probably do not need globals since I track them using the DATA type *)
type entry =
  (* | Fun of { t: Types.t; defined : bool; bytes_required : int } *)
  (* def_loc tracks in which function the symbol is located.
  If it is in None then it is a global object *)
  | Obj of { t : Types.t;  def_loc : string option}

(* Since every declaration has a unique name across functoins
we can just use a single map.
Otherwise we would need a map per function.
So another level of indirection. That would work as well *)
let symbol_table : (string, entry) Hashtbl.t = Hashtbl.create 16


(* Apply function f to entry in symbol table at key k *)
let modify k f =
  let old_entry = Hashtbl.find symbol_table k in
  Hashtbl.replace symbol_table k (f old_entry)

let add_var ?f_name name ty =
  Hashtbl.replace symbol_table name (Obj{ t = ty; def_loc=f_name })

  (* Right now does the smae thing but maybe keep stack frame size *)
(* let add_fun name ty = 
  Hashtbl.replace symbol_table name { t = ty } *)
let get name = Hashtbl.find symbol_table name
let get_opt name = Hashtbl.find_opt symbol_table name

let get_fun_locals f_name = 
  let find_locals k entry acc =
    match entry.def_loc with 
    | Some f_name' -> if f_name = f_name' then [k] else [] 
    | None -> []
  in
  Hashtbl.fold
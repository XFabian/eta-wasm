let counter = ref 0

(* Maybe have a per function counter like LLVM has *)
let make_temp () =
  let n = !counter in
  counter := n + 1;
  "tmp." ^ Int.to_string n

let make_named_tmp name =
  let n = !counter in
  counter := n + 1;
  name ^ "." ^ Int.to_string n

let make_label name =
  let n = !counter in
  counter := n + 1;
  name ^ Int.to_string n

(* Useful for if_else and if_end to have same number *)
let make_label_pair name1 name2 = 
  let n = !counter in
  counter := n + 1;
  name1 ^ Int.to_string n, name2 ^ Int.to_string n
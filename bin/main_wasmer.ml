(* If wasmer stuff would work *)

open Wasmer

(* Define our print_str functoin *)
let print_str _store args _ret =
  let val_int = Val.get_i32 (Val.Vec.get_element_const args 0) in
  print_string ("The result is " ^ Int32.to_string val_int);
  flush stdout;
  None

let () =
  (* Initialize Wasmer *)
  let engine = Engine.new_ () in
  let store = Store.new_ engine in
  let wasm = Wasmer.Util.load_wasm_file "add.wasm" in
  (* Load the WebAssembly module *)
  let real_module = Module.new_ store wasm in

  let print_int_functype = Wasmer.Util.( %-> ) Valkind.[ I32 ] [] in
  let print_int_func = Func.new_ store print_int_functype print_str in
  let imports = Extern.Vec.of_list [ Extern.of_func print_int_func ] in

  (* Create the instance *)
  let inst =
    match Instance.new_unsafe store real_module imports with
    | Error _ ->
        print_endline "> Error instanciating module!";
        failwith "Invalid module!"
    | Ok instance -> instance
  in
  (* Get exported functions of module *)
  let exports = Extern.Vec.make_new () in
  Instance.exports inst exports;

  let ( exported_funcs,
        (*exported_globals*) _,
        (*exported_tables*) _,
        exported_memories ) =
    Extern.Vec.split_kind exports
  in
  (* We do not define a memory for strings *)
  let run_func =
    match (exported_funcs, exported_memories) with
    | [ f ], [] -> f
    | _ ->
        print_endline "> Error accessing exports (no function)!";
        failwith "Invalid instance!"
  in

  (* Prepare function arguments *)
  let arg1 = Val.of_i32 1l in
  let arg2 = Val.of_i32 5l in

  let args = Val.Vec.of_list [ arg1; arg2 ] in
  let results = Val.Vec.make_empty_null () in

  match Func.call run_func args results with
  | Some trap ->
      print_endline "> Error calling the function!";
      let msg = Message.make_new () in
      Trap.message trap msg;
      print_endline (Message.to_string msg);
      failwith "Invalid function!"
  | None -> print_endline "Finshed"

(* print_endline (Int32.to_string (Val.get_i32 (Val.Vec.get_element_unsafe results 0))) *)

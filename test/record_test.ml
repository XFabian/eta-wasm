open Wetac
module StringMap = Map.Make (String)

(* Construct an entry in the Struct table for the lookup *)
let setup =
  let mems =
    StringMap.empty
    |> StringMap.add "x" Struct_table.{ member_t = Types.Int; offset = 0 }
    |> StringMap.add "y" Struct_table.{ member_t = Types.Int; offset = 8 }
  in
  (* let point_rec = "record Point{x,y :int}/n"
 *)
  Struct_table.add_struct "Point" Struct_table.{ members = mems };
  Symbols.add_local_var "p" (Record "Point")

let expect_failure f =
  try
    f ();
    print_endline "Typechecking succeeded unexpectedly"
  with
  | Failure msg -> print_endline ("Failure: " ^ msg)
  | exn -> print_endline ("Unexpected exception: " ^ Printexc.to_string exn)

(* TODO These needs to be fixed. They should all fail *)
let%expect_test "record_not_mem" =
  expect_failure (fun () ->
      let ast = Compile.parse_stmt "a : int = p.z" in
      let _t = Typecheck.typecheck_stmt [] ast in
      ());
  [%expect {| Failure: Record does not have member! |}]

let%expect_test "record_not_mem2" =
  expect_failure (fun () ->
      let ast = Compile.parse_stmt "a : int = p.x.z" in
      let _t = Typecheck.typecheck_stmt [] ast in
      ());
  [%expect {| Failure: Expression has to be Record Type! |}]

let%expect_test "record_undef" =
  expect_failure (fun () ->
      let ast = Compile.parse_stmt "p : Point5D" in
      let _ =
        ast
        |> Resolve.resolve_stmt (Resolve.StackMap.new_map ())
        |> snd
        |> Typecheck.typecheck_stmt []
      in
      ());
  [%expect {| Failure: Struct type is not defined |}]

let%expect_test "record_wrong_init" =
  expect_failure (fun () ->
      let ast = Compile.parse_stmt "p : Point = Point()" in
      let _t = Typecheck.typecheck_stmt [] ast in
      ());
  [%expect {| Failure: Calling undefined function! |}]

let%expect_test "record_wrong_assign" =
  expect_failure (fun () ->
      let ast = Compile.parse_stmt "p.x = true" in
      let _t = Typecheck.typecheck_stmt [] ast in
      ());
  [%expect {| Failure: Types of LHS and RHS are mismatched |}]

let%expect_test "record_dupl_member" =
  expect_failure (fun () ->
      let ast = Compile.parse_string "record Point3 { x : int; x : int }" in
      let _ = ast |> Resolve.resolve |> snd |> Typecheck.typecheck in
      ());
  [%expect {| Failure: Member already declared in record |}]

(* let%expect_test "record_member_arr_wrong" =
  expect_failure (fun () ->
      let ast =
        Compile.parse_string "record Point3 { x : int[][3]; y : int }"
      in
      let _ = ast |> Resolve.resolve |> snd |> Typecheck.typecheck in
      ());
  [%expect {| |}] *)

(* Except this one *)
let%expect_test "record_correct_assgn" =
  let ast = Compile.parse_stmt "p.x = 2" in
  let _t = Typecheck.typecheck_stmt [] ast in
  [%expect]

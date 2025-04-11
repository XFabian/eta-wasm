open Wetac

(* IMPORTANT I use the symbol table here to add a binding
Make sure that the test cases do not add bindings since this can 
have a influecnce on other tests 
*)
let f_type =
  Types.FunType
    {
      param_types = [ Types.Int; Types.Int ];
      ret_type = [ Types.Int; Types.Int ];
    }

let _ = Symbols.add_fun "test" f_type true
let _ = Symbols.add_local_var "x" Types.Int
let _ = Symbols.add_local_var "y" Types.Int
let _ = Symbols.add_local_var "z" Types.Int
let _ = Symbols.add_local_var "a" (Types.Array { elem_type = Types.Int })

let common_fail src =
  Printexc.record_backtrace false;
  let ast = Compile.parse_expr src in
  let _ = Typecheck.typecheck_expr ast in
  Expect_test_helpers_core.require_does_raise Lexing.dummy_pos (fun _a ->
      ignore (Typecheck.typecheck_expr ast))

let%expect_test "t_arr_lit_wrong" =
  common_fail "{1, true}";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "Not all elements in array literal have same type") |}]

let%expect_test "t_bin_wrong" =
  common_fail "1 + true";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "Tried to apply Binary op to wrong types!") |}]

let%expect_test "t_bin_wrong2" =
  common_fail "1 < true";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "Tried to apply Binary op to wrong types!") |}]

let%expect_test "t_arr_wrong2" =
  common_fail "{1,2} + {1}";
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "Array dimensions are requiered to be equal") |}]

let%expect_test "t_bin_true" =
  let ast = Compile.parse_expr "1 + 2" in
  let t = Typecheck.typecheck_expr ast in
  if t = [ Types.Int ] then [%expect {| |}] else [%expect.unreachable]

let%expect_test "t_arr_add_true" =
  let ast = Compile.parse_expr "{1} + {2}" in
  let t = Typecheck.typecheck_expr ast in
  if t = [ Types.Array { elem_type = Types.Int } ] then [%expect {| |}]
  else [%expect.unreachable]

let%expect_test "t_arr_add_comp_true" =
  let ast = Compile.parse_expr "{{1,2}} + {{3, 4}}" in
  let t = Typecheck.typecheck_expr ast in
  print_string (Types.show (List.hd t));
  [%expect {| Types.Array {elem_type = Types.Array {elem_type = Types.Int}} |}]

let%expect_test "m_assign_wrong" =
  let ast = Compile.parse_stmt "x, y = 1 + 3, 4, 5" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "Amount of lvalues and rhs values mismatch!") |}]

let%expect_test "m_assign_wrong2" =
  let ast = Compile.parse_stmt "x = test(1,2)" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "Amount of lvalues and rhs values mismatch!") |}]

let%expect_test "m_assign_arr_wrong3" =
  let ast = Compile.parse_stmt "a[2] = 1, 2" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure "Amount of lvalues and rhs values mismatch!") |}]

let%expect_test "m_assign_right" =
  let ast = Compile.parse_stmt "x, y, z = test(1,2), 2" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect]

let%expect_test "m_assign_right1" =
  let ast = Compile.parse_stmt "a[2] = 2" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect{||}]

let%expect_test "m_arr_assign1" =
  let ast = Compile.parse_stmt "a = {1,3,4}" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect]

let%expect_test "m_arr_assign2" =
  let ast = Compile.parse_stmt "a = {1,3,4} + {3, 4, 5}" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect]

let%expect_test "m_arr_assign_false" =
  let ast = Compile.parse_stmt "a = {true, false}" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure "Types of LHS and RHS are mismatched") |}]

let%expect_test "m_arr_decl" =
  let ast = Compile.parse_stmt "b_arr : int[] = {1,2}" in
  let _ = Typecheck.typecheck_stmt [] ast in
  Symbols.remove "b_arr";
  [%expect]

let%expect_test "m_arr_decl" =
  let ast = Compile.parse_stmt "b_arr : int[][] = {{1,2}, {3,4}}" in
  let _ = Typecheck.typecheck_stmt [] ast in
  Symbols.remove "b_arr";
  [%expect]

let%expect_test "m_arr_dec1" =
  let ast = Compile.parse_stmt "b_arr : int[4]" in
  let _ = Typecheck.typecheck_stmt [] ast in
  Symbols.remove "b_arr";
  [%expect]

let%expect_test "m_arr_dec_false" =
  let ast = Compile.parse_stmt "a : int[][4]" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "A specified dimension cannot follow after an unspecified one") |}]

let%expect_test "m_arr_dec_false1" =
  let ast = Compile.parse_stmt "a : int[4][][5]" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "A specified dimension cannot follow after an unspecified one") |}]

let%expect_test "m_arr_decl_false2" =
  let ast = Compile.parse_stmt "a : int[3] = {1,2}" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "Cannot specify dimensions and initialize array. Not allowed") |}]

let%expect_test "m_arr_dec_false4" =
  let ast = Compile.parse_stmt "a : int[true]" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure "Non Int value used as Index!") |}]

let%expect_test "m_arr_dec_false5" =
  let ast = Compile.parse_stmt "a : int[test(2, 3)]" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure "too many types for one index in array") |}]

let%expect_test "m_arr_dec_false7" =
  let ast = Compile.parse_stmt "b_arr : int[][] = {1,2}" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "Initializer has wrong type for the array") |}]

let%expect_test "m_arr_dec_false7" =
  let ast = Compile.parse_stmt "b_arr : int[][] = 2" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect.unreachable]
[@@expect.uncaught_exn
  {| (Failure "Initializer has wrong type for the array") |}]

let%expect_test "m_arr_lit" =
  let ast = Compile.parse_expr "{{1,2}, {3,4}}" in
  let t = Typecheck.typecheck_expr ast in
  Sexplib.Std.sexp_of_list Types.sexp_of_t t |> Expect_test_helpers_core.print_s;
  [%expect {| ((Array (elem_type (Array (elem_type Int))))) |}]

let%expect_test "m_arr_assign_ok" =
  let ast = Compile.parse_stmt "a[a[3]] = 4" in
  let _ = Typecheck.typecheck_stmt [] ast in
  [%expect]

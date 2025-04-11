open Wetac

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

let%expect_test "t_bin_true" =
  let ast = Compile.parse_expr "1 + 2" in
  let t = Typecheck.typecheck_expr ast in
  if t = Types.Int then [%expect {| |}] else [%expect.unreachable]

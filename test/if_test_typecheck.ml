open Wetac

let common_fail_stmt src =
  Printexc.record_backtrace false;
  let ast = Compile.parse_stmt src in
  Typecheck.typecheck_stmt [] ast



let%expect_test "t_if_guard_wrong_type" =
common_fail_stmt "if 2 {}";
[%expect.unreachable]
[@@expect.uncaught_exn {| (Failure "Guard of If has to have type Bool") |}]


let%expect_test "t_while_guard_wrong_type" =
common_fail_stmt "while 2 {}";
[%expect.unreachable]
[@@expect.uncaught_exn {| (Failure "Guard of While has to have type Bool") |}]
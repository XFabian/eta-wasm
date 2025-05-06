open Wetac

let point_rec = "record Point{x,y :int}/n"

(* TODO These needs to be fixed. They should all fail *)
let%expect_test "record_not_mem" =
  let ast = Compile.parse_expr "p : Point; a : int = p.z" in
  let t = Typecheck.typecheck_expr ast in
  if t = [ Types.Int ] then [%expect {| |}] else [%expect.unreachable]

let%expect_test "record_not_mem2" =
  let ast = Compile.parse_expr "p : Point; a : int = p.x.z" in
  let t = Typecheck.typecheck_expr ast in
  if t = [ Types.Int ] then [%expect {| |}] else [%expect.unreachable]

let%expect_test "record_undef" =
  let ast = Compile.parse_stmt "p : Point2D" in
  let t = Typecheck.typecheck_expr ast in
  if t = [ Types.Int ] then [%expect {| |}] else [%expect.unreachable]

let%expect_test "record_wrong_init" =
  let ast = Compile.parse_expr "p : Point = Point()" in
  let t = Typecheck.typecheck_expr ast in
  if t = [ Types.Int ] then [%expect {| |}] else [%expect.unreachable]

let%expect_test "record_wrong_assign" =
  let ast = Compile.parse_expr "p : Point; p.x = true" in
  let t = Typecheck.typecheck_expr ast in
  if t = [ Types.Int ] then [%expect {| |}] else [%expect.unreachable]

(* Except this one *)
let%expect_test "record_correct_assgn" =
  let ast = Compile.parse_expr "p : Point; p.x = 2" in
  let t = Typecheck.typecheck_expr ast in
  if t = [ Types.Int ] then [%expect {| |}] else [%expect.unreachable]

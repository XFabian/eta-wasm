open Wetac
(* Cram tests are an alternative. I think I want to tryto use them as well *)
(* Install pretty printer
#install_printer Sexplib.Sexp.pp_hum
 *)
(* Expect test helpers for S-Expressions
Expect_test_helpers_kernel.print_s *)
(* let%expect_test "addition" =
  (let ast = Compile.parse_file "test.eta" in
   [%sexp_of: Ast.prog] ast)
  |> Expect_test_helpers_core.print_s;
  [%expect {| 3 |}] *)

let%expect_test "simple" =
  (let ast = Compile.parse_file "test.eta" in
   [%sexp_of: Ast.prog] ast)
  |> Expect_test_helpers_core.print_s;
  [%expect
    {|
    (Program (
      (Global ((name x) (init ((Int 5))) (var_type Int)))
      (FunDecl (
        (name add)
        (params (a b))
        (f_type (FunType (param_types (Int Int)) (ret_type (Int))))
        (body (
          Block (
            (LocalDecl ((name y) (init ((Int 5))) (var_type Int)))
            (LocalDecl ((name f) (init ((Bool true))) (var_type Bool)))
            (LocalDecl ((name z) (init ()) (var_type Int)))
            (Assign
              (lhs ((Var y)))
              (rhs ((Int 7))))
            (Return ((
              Binary Add
              (Int 1)
              (Int 3)))))))))
      (FunDecl (
        (name empty)
        (params ())
        (f_type (
          FunType
          (param_types ())
          (ret_type    ())))
        (body (
          Block ((
            Return ((
              Binary Add
              (Int 3)
              (Int 4))))))))))) |}]

let%expect_test "simple_if" =
  (let ast = Compile.parse_file "if.eta" in
   [%sexp_of: Ast.prog] ast)
  |> Expect_test_helpers_core.print_s;
  [%expect
    {|
    (Program ((
      FunDecl (
        (name functionIf)
        (params (a))
        (f_type (
          FunType
          (param_types (Int))
          (ret_type    (Int))))
        (body (
          Block (
            (LocalDecl (
              (name x)
              (init ((Binary Add (Unary Neg (Int 1)) (Int 5))))
              (var_type Int)))
            (If
              (guard (Bool true))
              (then_br (
                Compound (
                  Block ((
                    Assign
                    (lhs ((Var a)))
                    (rhs ((Int 5))))))))
              (else_br ((
                Compound (
                  Block ((
                    Assign
                    (lhs ((Var a)))
                    (rhs ((Int 7))))))))))
            (Return ((Var a)))))))))) |}]

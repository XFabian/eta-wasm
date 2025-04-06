open Wasm_test

let print_position out_channel (lexbuf : Lexing.lexbuf) =
  let pos : Lexing.position = lexbuf.lex_curr_p in
  Printf.fprintf out_channel "%s:Line %d: Col %d Next token %s" pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
    (Lexing.lexeme lexbuf)

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast =
    try Parser.expr_top Lexer.read lexbuf with
    | Parser.Error ->
        Printf.fprintf stderr "Parsing Error at: %a\n" print_position lexbuf;
        exit (-1)
  in
  ast

let () =
  let ast = parse "2 + 3 * 4" in
  print_endline (Ast.show_expr ast);
  let wtac = Wtac_gen.gen ast in
  Wtac_print.print_wtac "test.c" wtac;
  Emit.emit wtac

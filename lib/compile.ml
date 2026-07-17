(* Will be called from main *)

let print_position out_channel (lexbuf : Lexing.lexbuf) =
  let pos : Lexing.position = lexbuf.lex_curr_p in
  Printf.fprintf out_channel "%s:Line %d: Col %d Next token %s" pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
    (Lexing.lexeme lexbuf)

let parse_string (s : string) : Ast.prog =
  let lexbuf = Lexing.from_string s in
  let ast =
    try Parser.prog Lexer.read lexbuf with
    | Parser.Error ->
        Printf.fprintf stderr "Parsing Error at: %a\n" print_position lexbuf;
        exit (-1)
  in
  ast

let parse_expr (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast =
    try Parser.expr_top Lexer.read lexbuf with
    | Parser.Error ->
        Printf.fprintf stderr "Parsing Error at: %a\n" print_position lexbuf;
        exit (-1)
  in
  ast

let parse_stmt (s : string) : Ast.stmt =
  let lexbuf = Lexing.from_string s in
  let ast =
    try Parser.stmt_top Lexer.read lexbuf with
    | Parser.Error ->
        Printf.fprintf stderr "Parsing Error at: %a\n" print_position lexbuf;
        exit (-1)
  in
  ast

let parse_file f_name =
  let inx = open_in f_name in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f_name };
  let ast =
    try Parser.prog Lexer.read lexbuf with
    | Parser.Error ->
        close_in inx;
        Printf.fprintf stderr "Parsing Error at: %a\n" print_position lexbuf;
        exit (-1)
  in
  close_in inx;
  ast

let compile stage src =
  let ast = parse_file src in
  if stage = Setting.Parse then print_endline (Ast.show_prog ast)
  else
    let _res_vars_state, res_ast = Resolve.resolve ast in
    Typecheck.typecheck res_ast;
    if stage = Setting.Validate then print_endline (Ast.show_prog res_ast)
    else
      let wtac = Wtac_gen.gen res_ast in
      if stage = Setting.Wtac then Wtac_print.print_wtac src wtac
      else if stage = Setting.Optimize then
        let opt_wtac = Optimizer.optimize wtac in
        Wtac_print.print_wtac (Filename.chop_extension src ^ "_opt.eta") opt_wtac
      else
        let wat_file = Filename.chop_extension src ^ ".wat" in
        Emit.emit wat_file wtac

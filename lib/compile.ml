(* Will be called from main *)

let print_position out_channel (lexbuf : Lexing.lexbuf) =
  let pos : Lexing.position = lexbuf.lex_curr_p in
  Printf.fprintf out_channel "%s:Line %d: Col %d Next token %s" pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
    (Lexing.lexeme lexbuf)

let _parse (s : string) : Ast.prog =
  let lexbuf = Lexing.from_string s in
  let ast =
    try Parser.prog Lexer.read lexbuf with
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
  print_endline (Ast.show_prog ast);
  if stage = Setting.Parse then ()
  else let res_ast = Resolve.resolve ast in 
   if stage = Setting.Validate then ()
  else ()
    (* let wtac = Wtac_gen.gen ast in
    Wtac_print.print_wtac "test.c" wtac;
    if stage = Setting.Wtac then () else Emit.emit wtac *)

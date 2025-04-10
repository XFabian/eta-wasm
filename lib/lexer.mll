{
    open Parser

    (* \a is\007 in Ocaml Char.chr 7 and /? is not allowed aswell
 The rest should be parsable by Scanf.unescaped *)
let drop_str n s = String.sub s n (String.length s - n)
  let unescape s =
    (* Map from escape sequences to corresponding unescaped characters;
     * use Ocaml quoted string literals to represent escape sequences so we
     * don't need to escape the backslashes *)
    let escapes =
      [
        (* Single quote *)
        ({|\'|}, Char.chr 39);
        (* Double quote *)
        ({|\"|}, Char.chr 34);
        (* Question mark *)
        ({|\?|}, Char.chr 63);
        (* Backslash *)
        ({|\\|}, Char.chr 92);
        (* Audible alert *)
        ({|\a|}, Char.chr 7);
        (* Backspace *)
        ({|\b|}, Char.chr 8);
        (* Form feed *)
        ({|\f|}, Char.chr 12);
        (* New line *)
        ({|\n|}, Char.chr 10);
        (* Carriage return*)
        ({|\r|}, Char.chr 13);
        (* Horizontal tab*)
        ({|\t|}, Char.chr 9);
        (* Vertical tab *)
        ({|\v|}, Char.chr 11);
      ]
    in
    let rec unescape_next remaining =
      if remaining = "" then []
      else
        let find_matching_escape (escape_seq, _) =
          String.starts_with ~prefix:escape_seq remaining
        in
        match List.find_opt find_matching_escape escapes with
        (* It's an escape sequence - add unescaped character to list *)
        | Some (escape_seq, unescaped) ->
            unescaped
            :: unescape_next
                 (drop_str (String.length escape_seq) remaining)
        | None -> remaining.[0] :: unescape_next (drop_str 1 remaining)
    in
    let unescaped_list = unescape_next s in
    String.of_seq (List.to_seq  unescaped_list)
}


let digit = ['0'-'9']
let int = digit+
let start_letter = ['a'-'z' 'A'-'Z' '_']
let cont_letter = ['a'-'z' 'A'-'Z' '_' '0' - '9']
let id = start_letter cont_letter*
let white = [' ' '\t']+
let newline = '\n' | "\r\n"

let string_lit =  ([^ '\"' '\\' '\n'] | '\\' [''' '"' '\\' '?' 'a' 'b' 'f' 'n' 'r' 't' 'v'])* 
rule read = 
    parse
    | white {read lexbuf}
    | newline {Lexing.new_line lexbuf; read lexbuf} 
    | "//" [^ '\n']* '\n' {Lexing.new_line lexbuf; read lexbuf}
    | "+" {PLUS}
    | "*" {MULT}
    | "-" {MINUS}
    | "/" {DIV}
    | "%" {MOD}
    | "<" {LT}
    | "<=" {LEQ}
    | ">" {GT}
    | ">=" {GEQ}
    | "==" {DEQUALS}
    | "!=" {NEQ}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | "{" {LBRACE}
    | "}" {RBRACE}
    | "[" {LBRACK}
    | "]" {RBRACK}
    | ":" {COLON}
    | "," {COMMA}
    | "=" {EQUAL}
    | "!" {BANG}
    | "|" {BAR}
    | "&" {AND} 
    | "_" {UNDERSCORE}
    | "int" {TINT}
    | "bool" {TBOOL}
    | "return" {RETURN}
    | "true" {TRUE}
    | "false" {FALSE}
    | "if" {IF}
    | "else" {ELSE}
    | "while" {WHILE}
    | id {VAR (Lexing.lexeme lexbuf)}
    | int {INT (int_of_string (Lexing.lexeme lexbuf))}
    | '"' {read_string (Buffer.create 17) lexbuf }
    | eof {EOF}
    | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }


(* See https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html *)
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { print_endline (Lexing.lexeme lexbuf); Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (failwith ("String is not terminated")) }

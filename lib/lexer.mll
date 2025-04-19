{
    open Parser
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
    | ";" {SEMICOLON}
    | "=" {EQUAL}
    | "!" {BANG}
    | "|" {BAR}
    | "&" {AND} 
    | "_" {VAR "_"}
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
  | "\\x{" (['0'-'9' 'a'-'f' 'A'-'F']+ as hex) '}' {
          if String.length hex > 6 then
          failwith "Unicode escape mut have at most 6 he digits"
          else 
          let c = "0x" ^ hex |> int_of_string |> Stdlib.Uchar.of_int in
          (* TODO not working currently but not sure why *)
          if not ( Stdlib.Uchar.is_valid (Stdlib.Uchar.to_int c)) then
          failwith "Unicode character is not in the valid range"
          else
          Buffer.add_utf_8_uchar buf c; read_string buf lexbuf 
          }
  | [^ '"' '\\']+
    { print_endline (Lexing.lexeme lexbuf); Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (failwith ("String is not terminated")) }

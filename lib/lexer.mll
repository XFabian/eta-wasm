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


rule read = 
    parse
    | white {read lexbuf}
    | newline {Lexing.new_line lexbuf; read lexbuf} 
    | "+" {PLUS}
    | "*" {MULT}
    | "-" {MINUS}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | id {VAR (Lexing.lexeme lexbuf)}
    | int {INT (int_of_string (Lexing.lexeme lexbuf))}
    | eof {EOF}
    | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }




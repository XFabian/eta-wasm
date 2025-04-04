%{
    open Ast
%}

%token <string> VAR
%token <int> INT
%token PLUS MULT MINUS
%token LPAREN RPAREN
%token EOF


%left PLUS MINUS
%left MULT

%start <Ast.expr> expr_top
%%

expr_top : 
    | e = expr ; EOF {e}

expr:
    | i = INT {Int i}
    | e1 = expr; b = binop; e2 = expr {Binary (b, e1, e2)}
    | LPAREN e = expr RPAREN {e}

%inline
binop: 
    | PLUS {Add}
    | MINUS {Sub}
    | MULT {Mult}
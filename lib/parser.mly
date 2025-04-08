%{
    open Ast
%}

%token <string> VAR
%token <int> INT
%token PLUS MULT MINUS
%token TRUE FALSE
%token LPAREN RPAREN LBRACE RBRACE
%token COLON COMMA EQUAL
%token TINT TBOOL
%token RETURN
%token EOF


%left PLUS MINUS
%left MULT

%start <Ast.prog> prog
%%

prog : 
    | decls = list(declarations); EOF {Program decls}
    ;
declarations: 
    | func = func_decl {FunDecl func}
    | var = var_decl {Global var}
    ;
func_decl:
    | name = VAR; LPAREN; params_and_types_opt=option(params) ; RPAREN; ret_type = option(type_spec);
    LBRACE; body = block ; RBRACE {
        let params, types = match params_and_types_opt with 
        | Some params_and_types -> List.split params_and_types  
        | None -> [], []
        in 
        let f_type = Types.FunType {param_types=types; ret_type} in
        {name; params; f_type; body}
    }
    // a:int, b:int or just empty
params:
    | params_and_types= separated_list(COMMA, name= VAR ty= type_spec {(name, ty)})
    {params_and_types}
    ;

var_decl:
    | name = VAR; ty = type_spec ; init=option(EQUAL e = expr_top {e})
    {{name;init; var_type=ty}}
    ;
type_spec:
    | COLON; TINT {Types.Int}
    | COLON; TBOOL {Types.Bool}

block:
    | block_items = list(stmt) {Block block_items}
stmt: 
    | RETURN ; e = expr_top {Return e}
    | name = VAR; ty = type_spec init = option(EQUAL e=expr_top {e})
    {LocalDecl {name; init; var_type=ty}}
    | lhs = expr_top ; EQUAL; rhs = expr_top {Assign {lhs; rhs}}
    ;

expr_top : 
    | e = expr {e}

expr:
    | name = VAR {Var name}
    | i = INT {Int i}
    | TRUE {Bool true}
    | FALSE {Bool false} 
    | e1 = expr; b = binop; e2 = expr {Binary (b, e1, e2)}
    | LPAREN e = expr RPAREN {e}

%inline
binop: 
    | PLUS {Add}
    | MINUS {Sub}
    | MULT {Mult}
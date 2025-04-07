%{
    open Ast
%}

%token <string> VAR
%token <int> INT
%token PLUS MULT MINUS
%token LPAREN RPAREN LBRACE RBRACE
%token COLON COMMA
%token TINT
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
    ;
func_decl:
    | name = VAR; LPAREN; params_and_types_opt=option(params) ; RPAREN; ret_type = option(type_spec);
    LBRACE; body = expr_top ; RBRACE {
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

type_spec:
    | COLON; TINT {Types.Int}
expr_top : 
    | e = expr {e}

expr:
    | i = INT {Int i}
    | e1 = expr; b = binop; e2 = expr {Binary (b, e1, e2)}
    | LPAREN e = expr RPAREN {e}

%inline
binop: 
    | PLUS {Add}
    | MINUS {Sub}
    | MULT {Mult}
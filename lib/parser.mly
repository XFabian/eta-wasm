%{
    open Ast

let string_to_int_arr s = 
  s 
  |> String.to_seq
  |> Seq.map Char.code
  |> Seq.map (fun i -> Int i)
  |> List.of_seq
%}

%token <string> VAR
%token <int> INT
%token <string> STRING
%token PLUS MULT MINUS DIV MOD 
%token LT GT LEQ GEQ
%token NEQ DEQUALS
%token TRUE FALSE
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token COLON COMMA EQUAL SEMICOLON
%token TINT TBOOL
%token RETURN IF ELSE WHILE
%token BANG BAR AND
%token RECORD DOT
%token EOF

%nonassoc EQUAL
%left BAR
%left AND
%left DEQUALS NEQ
%left LEQ LT GEQ GT
%left PLUS MINUS
%left MULT DIV MOD

%nonassoc BANG
%nonassoc IF
%nonassoc ELSE

%start <Ast.stmt> stmt_top
%start <Ast.expr> expr_top
%start <Ast.prog> prog
%%

separated_nonempty_list_option_trailing(sep, X):
 | x = X {[x]}
 | x = X; sep; xs= separated_nonempty_list_option_trailing(sep, X) 
 {[x] @ xs}
 | x = X sep; {[x]}
 ;


prog : 
    | decls = list(declarations); EOF {Program decls}
    ;
declarations: 
    | func = func_decl {FunDecl func}
    | var = var_decl {Global var}
    | rec_d = rec_decl {RecordDecl rec_d}
    ;
func_decl:
    | name = VAR; LPAREN; params_and_types=params ; RPAREN; ret_type_opt = option(COLON ts= separated_list(COMMA, type_spec) {ts});
    LBRACE; body = block ; RBRACE {
        let params, types = match params_and_types with 
        | [] -> [], []
        | params_and_types -> List.split params_and_types  
        in 
        let ret_type = match ret_type_opt with 
        | Some rs -> rs
        | None -> []
        in 
        let f_type = Types.FunType {param_types=types; ret_type} in
        {name; params; f_type; body}
    }
    // a:int, b:int or just empty
params:
    | params_and_types= separated_list(COMMA, name= VAR COLON ty= type_spec {(name, ty)})
    {params_and_types}
    ;

var_decl:
    | name = VAR; COLON ty = type_spec ; EQUAL e = expr
    {{name;init=Some e; var_type=ty}}
    | name = VAR; COLON ty = type_spec 
    {{name;init = None; var_type=ty}}
    ;
type_spec:
    | t = type_spec arrs = nonempty_list(LBRACK option(expr) RBRACK {}) 
    {   
        let rec parse_array arr base = match arr with 
        | [] -> base 
        | _::xs -> let new_t = Types.Array{elem_type=base} in 
        parse_array xs new_t
        in 
        parse_array arrs t
    }
    | prim_type {$1}

prim_type:
    | TINT {Types.Int}
    | TBOOL {Types.Bool}
    | x = VAR {Types.Record x}
block:
    | block_items = list(stmt_opt_semi) {Block block_items}

rec_decl:
    | RECORD tag = VAR; LBRACE; members = list(member_decl) RBRACE 
    {
        let members_f = List.flatten members in
        {tag; members=members_f}
    }
    ;

member_decl:
    | names = separated_nonempty_list(COMMA, VAR); COLON ; t = type_spec; option(SEMICOLON)
    {
        List.map (fun name -> {name; t}) names
    }
    | name= VAR; COLON; t= type_spec; option(SEMICOLON) {[{name; t}]}
    ;

stmt_top:
    | s =stmt; EOF {s}

stmt_opt_semi:
    | stmt {$1}
    | stmt; SEMICOLON {$1}
stmt: 
    | RETURN  es = separated_list(COMMA, expr) {Return es}
    | arr_decl_stmt {$1}
    // Not arrats
    | name = VAR; COLON; ty = prim_type init = option(EQUAL e=expr {e}) %prec EQUAL
    {LocalDecl {name; init; var_type=ty}}
    
    | lhs = separated_nonempty_list(COMMA, lval); EQUAL; rhs = separated_nonempty_list(COMMA, expr) %prec EQUAL {Assign {lhs; rhs}}
    
    | IF guard = expr; then_br = stmt; else_br = ioption(ELSE s=stmt {s}) 
    {If {guard; then_br; else_br}}
    | WHILE guard= expr; body=stmt {While {guard; body }}
    | LBRACE; block = list(stmt); RBRACE {Compound (Block block)}
    | name = VAR LPAREN; args=separated_list(COMMA,expr) RPAREN {Procedure {name; args}}
    ;
lval:
    | name = VAR {E (Var name)}
    | arr = lval LBRACK e = expr RBRACK {
        let arr_e = match arr with 
        | E e -> e 
        | V _ -> failwith "A declaration cannot be indexed"
        in 
        E (Subscript(arr_e, e))}
    | rec_v = lval DOT x = VAR {
        let rec_e = match rec_v with 
        | E e -> e 
        | V _ -> failwith "A Declarartion cannot be dotted"
        in
        E (Dot(rec_e, x))}
    | name = VAR COLON t = type_spec {V {name; init=None; var_type=t}}
    // | postfix_expr  {E ($1)}
    ;

arr_decl_stmt:
    | name = VAR; COLON; pt= prim_type; 
    ar= nonempty_list(LBRACK e=option(expr) RBRACK {e});
    init = option(EQUAL e = expr {e})
    {
        let rec parse_array arr base dims = (match arr with 
        | [] -> base, List.rev dims
        | x::xs -> let new_t = Types.Array{elem_type=base} in
        let dims'= x::dims 
        in
        parse_array xs new_t dims')
        in 
        let a_type, dims = parse_array ar pt [] 
        in
        ArrayDecl{name; a_type; dims; init}
    }




arr_sub: 
    | LBRACK e = expr RBRACK {e}  
    | arr = arr_sub LBRACK e= expr RBRACK {Subscript(arr, e)}  
// ----------------------------------
expr_top:
    | e =expr ; EOF {e}
    

expr : 
    | unary_expr {$1} 
    | e1 = expr; b = binop; e2 = expr {Binary (b, e1, e2)}

unary_expr: 
    | op = unary_op; e = expr  {Unary(op, e)}
    | postfix_expr {$1}
    // b[1].x has to be allowed
postfix_expr:
    | primary_expr {$1}
    | e = postfix_expr; LBRACK e1= expr RBRACK {Subscript(e, e1)}
    | e = postfix_expr; DOT x = VAR {Dot (e, x)}
primary_expr:
    | name = VAR {Var name}
    | i = INT {Int i}
    | TRUE {Bool true}
    | FALSE {Bool false}
    | a = arrayLit {a}
    | s = STRING {Ast.ArrayLit (string_to_int_arr s)}
    | name = VAR; LPAREN args=separated_list(COMMA, expr) RPAREN  {FunCall{name; args}}
    | LPAREN e = expr RPAREN {e}


arrayLit:
| LBRACE e_list = separated_nonempty_list_option_trailing(COMMA, expr) RBRACE {ArrayLit e_list} 
| LBRACE RBRACE {ArrayLit []}


%inline binop: 
    | PLUS {Add}
    | MINUS {Sub}
    | MULT {Mult}
    | DIV {Div}
    | MOD {Mod}
    | AND {And}
    | BAR {Or}
    | LT {Lt}
    | LEQ {Leq}
    | GT {Gt}
    | GEQ {Geq}
    | DEQUALS {Eq}
    | NEQ {Neq}

%inline unary_op:
    | BANG {Not}
    | MINUS {Neg}
%{
open Ast
open Make_ast
%}

%token EOF

%token <int> INT
%token <string> ID
%token <float> FLOAT

(* Keywords *)
%token LET IF FOR TRUE FALSE INT_ANNOTATION 
       BOOL_ANNOTATION IDX_ANNOTATION FLOAT_ANNOTATION
       UNROLL MEMORY BANK FUNC TYPE

(* Parentheses, brackets, etc *)
%token LPAREN RPAREN LBRACK RBRACK LSQUARE RSQUARE FORWARD_SLASH

(* Operations *)
%token EQUAL NEQ GEQ LEQ LT GT EQ PLUS MINUS TIMES
       AND OR REASSIGN 

(* Other *)
%token SEMICOLON COLON RANGE_DOTS COMMA

(* Precedences *)
%right OR
%right AND
%left NEQ GEQ LEQ LT GT EQ 
%left PLUS MINUS
%left TIMES

%start <Ast.command> prog

%%

prog:
  | c = cmd; EOF
    { c } ;

cmd:
  | f = ID LPAREN a = args RPAREN SEMICOLON
    { make_app f a }
  | TYPE tname = ID EQUAL tval = type_annotation (* FIXME *)
    { make_typedef tname tval }
  | FUNC f = ID LPAREN a = annotated_args RPAREN 
    LBRACK body = cmd RBRACK
    { make_function f a body }
  | MEMORY x = ID COLON t = type_annotation LSQUARE s = INT RSQUARE BANK 
    LPAREN b = INT RPAREN SEMICOLON
    { make_assignment x (make_array s b t) }
  | LET x = ID EQUAL e1 = expr SEMICOLON
    { make_assignment x e1 }
  | IF LPAREN b = expr RPAREN LBRACK body = cmd RBRACK
    { make_if b body }
  | e1 = expr; REASSIGN; e2 = expr; SEMICOLON
    { make_reassignment e1 e2 } 
  | FOR LPAREN LET x = ID EQUAL x1 = expr RANGE_DOTS x2 = expr RPAREN UNROLL u = INT
    LBRACK c = cmd RBRACK
    { make_for_impl x x1 x2 u c }
  | FOR LPAREN LET x = ID EQUAL x1 = expr RANGE_DOTS x2 = expr RPAREN 
    LBRACK c = cmd RBRACK
    { make_for x x1 x2 c } ;
  | FOR LPAREN LET i = ID COMMA j = ID EQUAL x1 = expr RANGE_DOTS x2 = expr RPAREN u = INT
    LBRACK c = cmd RBRACK
    { make_for_double i j x1 x2 u c }
  | c1 = cmd c2 = cmd
    { make_seq c1 c2 }

expr:
  | x = ID; LSQUARE; idx = expr; RSQUARE
    { make_array_access_impl x idx }
  | x = ID; LSQUARE; idx1 = expr; RSQUARE; LSQUARE; idx2 = expr; RSQUARE
    { make_array_access_expl x idx1 idx2 }
  | LPAREN e = expr RPAREN
    { e }
  | e1 = expr; bop = binop; e2 = expr
    { make_binop bop e1 e2 } ;
  | x = ID LSQUARE index = expr RSQUARE
    { make_array_access x index }
  | x = INT
    { make_int x }
  | f = FLOAT
    { make_float f }
  | TRUE
    { make_bool true }
  | FALSE
    { make_bool false }
  | x = ID
    { make_var x }

annotated_id:
  | x = ID COLON t = type_annotation
    { (x, t) } 

annotated_args:
  | a = separated_list(COMMA, annotated_id ) { a }

args:
  | a = separated_list(COMMA, expr) { a }

type_annotation:
  | BOOL_ANNOTATION { TBool }
  | INT_ANNOTATION { TInt None }
  | FLOAT_ANNOTATION { TFloat } 
  | t = type_annotation LSQUARE s = INT RSQUARE BANK LPAREN bf = INT RPAREN 
    { TArray (t, bf, s) }
  | t = type_annotation LSQUARE s = INT RSQUARE 
    { TArray (t, 1, s) }
  | x = ID { TAlias x }

%inline binop:
  | NEQ { BopNeq }
  | GEQ { BopGeq }
  | LEQ { BopLeq }
  | LT  { BopLt }
  | GT  { BopGt }
  | EQ  { BopEq }
  | PLUS { BopPlus }
  | MINUS { BopMinus }
  | TIMES { BopTimes }
  | AND { BopAnd }
  | OR { BopOr } ;


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
       BOOL_ANNOTATION FLOAT_ANNOTATION
       MUX UNROLL BANK FUNC TYPE

(* Parentheses, brackets, etc *)
%token LPAREN RPAREN LBRACK RBRACK LSQUARE RSQUARE

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
  | MUX s = INT; mid = ID; LPAREN; aid = ID; RPAREN; SEMICOLON
    { make_muxdef s mid aid }
  | f = ID LPAREN a = args RPAREN SEMICOLON
    { make_app f a }
  | TYPE tname = ID EQUAL tval = type_annotation SEMICOLON
    { make_typedef tname tval }
  | FUNC f = ID LPAREN a = annotated_args RPAREN
    LBRACK body = cmd RBRACK
    { make_function f a body }
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
  | c1 = cmd c2 = cmd
    { make_seq c1 c2 }

expr:
  | x = ID; a = access;
    { make_aa x a }
  | x = ID; LBRACK; idx1 = expr; RBRACK; LSQUARE; idx2 = expr; RSQUARE
    { make_banked_aa x idx1 idx2 }
  | LPAREN e = expr RPAREN
    { e }
  | e1 = expr; bop = binop; e2 = expr
    { make_binop bop e1 e2 } ;
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

array_def:
  | LSQUARE; s = INT; BANK; LPAREN; bf = INT; RPAREN; RSQUARE; t = array_def
    { (s, bf) :: t }
  | LSQUARE; s = INT; RSQUARE; t = array_def
    { (s, 1) :: t }
  | LSQUARE; s = INT; BANK; LPAREN; bf = INT; RPAREN; RSQUARE
    { [(s, bf)] }
  | LSQUARE; s = INT; RSQUARE
    { [(s, 1)] }

access:
  | LSQUARE; idx = expr; RSQUARE
    { [idx] }
  | LSQUARE; idx = expr; RSQUARE; a = access
    { idx :: a }

type_annotation:
  | BOOL_ANNOTATION { TBool }
  | INT_ANNOTATION { TIndex ((0, 1), (min_int, max_int)) }
  | FLOAT_ANNOTATION { TFloat }
  | t = type_annotation a = array_def
    { TArray (t, a) }
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


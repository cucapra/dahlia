%{
open Ast
open Make_ast
%}

%token EOF

%token <int> INT
%token <string> ID

(* Keywords *)
%token LET UNROLL FOR TRUE FALSE INT_ANNOTATION 
       BOOL_ANNOTATION IF

(* Parentheses, brackets, etc *)
%token LPAREN RPAREN LBRACK RBRACK LSQUARE RSQUARE

(* Operations *)
%token EQUAL NEQ GEQ LEQ LT GT EQ PLUS MINUS TIMES
       AND OR REASSIGN 

(* Other *)
%token SEMICOLON RANGE_DOTS


%left AND OR
%left NEQ GEQ LEQ LT GT EQ
%left PLUS MINUS
%left TIMES

%left EQUAL

%start <Ast.command> prog

%%

prog:
  | c = cmd; EOF
    { c } ;

cmd:
  | c1 = cmd c2 = cmd
    { make_seq c1 c2 }
  | LET x = ID EQUAL e1 = expr SEMICOLON
    { make_assignment x e1 }
  | IF LPAREN b = expr RPAREN LBRACK body = cmd RBRACK
    { make_if b body }
  | id = ID LSQUARE index = expr RSQUARE REASSIGN e = expr SEMICOLON
    { make_array_update id index e }
  | t = type_annotation x = ID LSQUARE s = INT RSQUARE SEMICOLON
    { make_assignment x (make_array s t) }
  | FOR LPAREN LET x = ID EQUAL x1 = expr RANGE_DOTS x2 = expr RPAREN 
    LBRACK c = cmd RBRACK
    { make_for x x1 x2 c } ;

expr:
  | LPAREN e = expr RPAREN
    { e }
  | x = ID LSQUARE index = expr RSQUARE
    { make_array_access x index }
  | x = INT
    { make_int x }
  | TRUE
    { make_bool true }
  | FALSE
    { make_bool false }
  | x = ID
    { make_var x }
  | e1 = expr bop = binop e2 = expr
    { make_binop bop e1 e2 } ;

type_annotation:
  | BOOL_ANNOTATION { TBool }
  | INT_ANNOTATION { TInt } ;

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


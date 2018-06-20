%{
open Ast
open Make_ast
%}

%token EOF

%token <int> INT
%token <string> ID

(* Keywords *)
%token LET IF FOR TRUE FALSE INT_ANNOTATION 
       BOOL_ANNOTATION IDX_ANNOTATION UNROLL 
       MEMORY BANK

(* Parentheses, brackets, etc *)
%token LPAREN RPAREN LBRACK RBRACK LSQUARE RSQUARE FORWARD_SLASH

(* Operations *)
%token EQUAL NEQ GEQ LEQ LT GT EQ PLUS MINUS TIMES
       AND OR REASSIGN 

(* Other *)
%token SEMICOLON COLON RANGE_DOTS

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
  | MEMORY x = ID COLON t = type_annotation LSQUARE s = INT RSQUARE BANK 
    LPAREN b = INT RPAREN SEMICOLON
    { make_assignment x (make_array s b t) }
  | LET x = ID EQUAL e1 = expr SEMICOLON
    { make_assignment x e1 }
  | IF LPAREN b = expr RPAREN LBRACK body = cmd RBRACK
    { make_if b body }
  | e1 = expr; REASSIGN; e2 = expr; SEMICOLON
    { make_reassignment e1 e2 } 
  | FOR LPAREN LET x = ID EQUAL x1 = expr RANGE_DOTS x2 = expr RPAREN UNROLL u = expr  
    LBRACK c = cmd RBRACK
    { make_for_impl x x1 x2 u c }
  | FOR LPAREN LET x = ID EQUAL x1 = expr RANGE_DOTS x2 = expr RPAREN 
    LBRACK c = cmd RBRACK
    { make_for x x1 x2 c } ;
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
  | TRUE
    { make_bool true }
  | FALSE
    { make_bool false }
  | x = ID
    { make_var x }

type_annotation:
  | BOOL_ANNOTATION { TBool }
  | INT_ANNOTATION { TInt None } ;

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


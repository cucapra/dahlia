%{
open Ast
open Make_ast
%}

%token EOF

%token <int> INT
%token <string> ID

%token LET
%token EQUAL
%token SEMICOLON

%token FOR

%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK

%token NEQ
%token GEQ
%token LEQ
%token LT
%token GT
%token EQ

%token PLUS
%token MINUS
%token TIMES

%token TRUE
%token FALSE

%token RANGE_DOTS

%left EQUAL

%start <Ast.command list> prog

%%

prog:
  | e = separated_list(SEMICOLON, cmd); EOF
    { e } ;

cmd:
  | FOR; LPAREN; LET; x = ID; EQUAL; x1 = INT; RANGE_DOTS; x2 = INT; RPAREN; 
    LBRACK; e = separated_list(SEMICOLON, cmd); RBRACK
    { make_for x x1 x2 e }
  | LET; x = ID; EQUAL; e1 = expr
    { make_assignment x e1 } ;

expr:
  | x = INT
    { make_int x }
  | TRUE
    { make_bool true }
  | FALSE
    { make_bool false }
  | x = ID
    { make_var x }
  | e1 = expr; bop = binop; e2 = expr
    { make_binop bop e1 e2 } ;

%inline binop:
  | NEQ { BopNeq }
  | GEQ { BopGeq }
  | LEQ { BopLeq }
  | LT  { BopLt }
  | GT  { BopGt }
  | EQ  { BopEq }
  | PLUS { BopPlus }
  | MINUS { BopMinus }
  | TIMES { BopTimes } ;

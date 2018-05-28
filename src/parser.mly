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

%token NEQ
%token GEQ
%token LEQ
%token LT
%token GT
%token EQ

%token TRUE
%token FALSE

%token RANGE_DOTS

%start <Ast.command list> prog

%%

prog:
  | e = separated_list(SEMICOLON, cmd); EOF
    { e } ;

cmd:
  | LET; x = ID; EQUAL; e1 = expr
    { make_assignment x e1 } ;

expr:
  | x1 = INT; RANGE_DOTS; x2 = INT
    { make_range x1 x2 }
  | x = INT
    { make_int x }
  | TRUE
    { make_bool true }
  | FALSE
    { make_bool false } ;
  | e1 = expr; bop = binop; e2 = expr
    { make_binop bop e1 e2 }

%inline binop:
  | NEQ { BopNeq }
  | GEQ { BopGeq }
  | LEQ { BopLeq }
  | LT  { BopLt }
  | GT  { BopGt }
  | EQ  { BopEq } ;

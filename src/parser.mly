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

%start <Ast.command list> prog

%%

prog:
  | e = separated_list(SEMICOLON, cmd); EOF
    { e } ;

cmd:
  | LET; x = ID; EQUAL; e1 = expr
    { make_assignment x e1 }

expr:
  | x = INT
    { make_int x }


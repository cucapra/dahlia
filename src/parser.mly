%{
open Ast
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
  | cmd EOF { $1 }

cmd:
  | MUX INT ID LPAREN ID RPAREN SEMICOLON                   { CMuxDef ($3, $5, $2) }
  | ID LPAREN args RPAREN SEMICOLON                         { CApp ($1, $3) }
  | TYPE ID EQUAL type_annotation SEMICOLON                 { CTypeDef ($2, $4) }
  | FUNC ID LPAREN annotated_args RPAREN LBRACK cmd RBRACK  { CFuncDef ($2, $4, $7) }
  | LET ID EQUAL expr SEMICOLON                             { CAssign ($2, $4) }
  | IF LPAREN expr RPAREN LBRACK cmd RBRACK                 { CIf ($3, $6) }
  | expr REASSIGN expr SEMICOLON                            { CReassign ($1, $3) }
  | FOR LPAREN LET x = ID EQUAL x1 = expr RANGE_DOTS x2 = expr RPAREN UNROLL u = INT
    LBRACK c = cmd RBRACK                                   { CFor (x, x1, x2, u, c) }
  | FOR LPAREN LET x = ID EQUAL x1 = expr RANGE_DOTS x2 = expr RPAREN
    LBRACK c = cmd RBRACK                                   { CFor (x, x1, x2, 1, c) }
  | cmd cmd                                                 { CSeq ($1, $2) }

expr:
  | ID access                                   { EAA ($1, $2) }
  | ID LBRACK expr RBRACK LSQUARE expr RSQUARE  { EBankedAA ($1, $3, $6) }
  | LPAREN expr RPAREN                          { $2 }
  | expr binop expr                             { EBinop ($2, $1, $3) }
  | INT                                         { EInt ($1, true) }
  | FLOAT                                       { EFloat $1 }
  | TRUE                                        { EBool true }
  | FALSE                                       { EBool false }
  | ID                                          { EVar $1 }

annotated_id:
  | ID COLON type_annotation { ($1, $3) }

annotated_args:
  | separated_list(COMMA, annotated_id ) { $1 }

args:
  | separated_list(COMMA, expr) { $1 }

array_def:
  | LSQUARE INT BANK LPAREN INT RPAREN RSQUARE array_def  { ($2, $5) :: $8 }
  | LSQUARE INT RSQUARE array_def                         { ($2, 1) :: $4 }
  | LSQUARE INT BANK LPAREN INT RPAREN RSQUARE            { [($2, $5)] }
  | LSQUARE INT RSQUARE                                   { [($2, 1)] }

access:
  | LSQUARE expr RSQUARE          { [$2] }
  | LSQUARE expr RSQUARE access   { $2 :: $4 }

type_annotation:
  | BOOL_ANNOTATION             { TBool }
  | INT_ANNOTATION              { TIndex ((0, 1), (min_int, max_int)) }
  | FLOAT_ANNOTATION            { TFloat }
  | type_annotation array_def   { TArray ($1, $2) }
  | ID                          { TAlias $1 }

%inline binop:
  | NEQ     { BopNeq }
  | GEQ     { BopGeq }
  | LEQ     { BopLeq }
  | LT      { BopLt }
  | GT      { BopGt }
  | EQ      { BopEq }
  | PLUS    { BopPlus }
  | MINUS   { BopMinus }
  | TIMES   { BopTimes }
  | AND     { BopAnd }
  | OR      { BopOr }


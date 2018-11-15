%{
open Ast

let parse_sequence c1 c2 =
  match c1, c2 with
  | CSeq cmds1, CSeq cmds2 -> CSeq (cmds1 @ cmds2)
  | c, CSeq cmds           -> CSeq (c :: cmds)
  | CSeq cmds, c           -> CSeq (cmds @ [c])
  | c1, c2                 -> CSeq ([c1; c2])

%}

%token EOF

%token <int> INT
%token <string> ID
%token <float> FLOAT

(* Keywords *)
%token LET IF FOR TRUE FALSE BOOL_ANNOTATION
       FLOAT_ANNOTATION MUX UNROLL BANK FUNC
       TYPE WRITE READ AS BIT VIEW

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
  | acmd      { $1 }
  | acmd cmd  { parse_sequence $1 $2 }

acmd:
  | WRITE expr AS ID SEMICOLON                              { CCap(Write, $2, $4)}
  | READ expr AS ID SEMICOLON                               { CCap(Read, $2, $4)}
  | MUX INT ID LPAREN ID RPAREN SEMICOLON                   { CMuxDef ($3, $5, $2) }
  | ID LPAREN args RPAREN SEMICOLON                         { CApp ($1, $3) }
  | TYPE ID EQUAL type_annotation SEMICOLON                 { CTypeDef ($2, $4) }
  | FUNC ID LPAREN annotated_args RPAREN LBRACK cmd RBRACK  { CFuncDef ($2, $4, $7) }
  | LET ID EQUAL expr SEMICOLON                             { CAssign ($2, $4) }
  | IF LPAREN expr RPAREN LBRACK cmd RBRACK                 { CIf ($3, $6) }
  | expr REASSIGN expr SEMICOLON                            { CReassign ($1, $3) }
  | FOR LPAREN LET ID EQUAL expr RANGE_DOTS expr RPAREN UNROLL INT
    LBRACK cmd RBRACK                                       { CFor ($4, $6, $8, $11, $13) }
  | FOR LPAREN LET ID EQUAL expr RANGE_DOTS expr RPAREN
    LBRACK cmd RBRACK                                       { CFor ($4, $6, $8, 1, $11) }
  | expr SEMICOLON                                          { CExpr $1 }

expr:
  | VIEW ID LSQUARE INT COLON INT COLON INT RSQUARE { EView ($2, $4, $6, $8) }
  | ID access                                       { EAA ($1, $2) }
  | ID LBRACK expr RBRACK LSQUARE expr RSQUARE      { EBankedAA ($1, $3, $6) }
  | LPAREN expr RPAREN                              { $2 }
  | expr binop expr                                 { EBinop ($2, $1, $3) }
  | INT                                             { EInt $1 }
  | FLOAT                                           { EFloat $1 }
  | TRUE                                            { EBool true }
  | FALSE                                           { EBool false }
  | ID                                              { EVar $1 }

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

bit_annotation:
  | BIT LT INT GT { $3 }

basic_type:
  | bit_annotation              { TIndex ((0, 1), (0, Core.Int.pow 2 $1)) }
  | BOOL_ANNOTATION             { TBool }
  | FLOAT_ANNOTATION            { TFloat }
  | ID                          { TAlias $1 }

type_annotation:
  | basic_type             { $1 }
  | basic_type array_def   { TArray ($1, $2) }

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


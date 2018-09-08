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
  | c = cmd; EOF
    { c } ;

cmd:
  | MUX s = INT; mid = ID; LPAREN; aid = ID; RPAREN; SEMICOLON
    { CMuxDef (mid, aid, s) }
  | f = ID LPAREN a = args RPAREN SEMICOLON
    { CApp (f, a) }
  | TYPE tname = ID EQUAL tval = type_annotation SEMICOLON
    { CTypeDef (tname, tval) }
  | FUNC f = ID LPAREN a = annotated_args RPAREN
    LBRACK body = cmd RBRACK
    { CFuncDef (f, a, body) }
  | LET x = ID EQUAL e1 = expr SEMICOLON
    { CAssign (x, e1) }
  | IF LPAREN b = expr RPAREN LBRACK body = cmd RBRACK
    { CIf (b, body) }
  | e1 = expr; REASSIGN; e2 = expr; SEMICOLON
    { CReassign (e1, e2) }
  | FOR LPAREN LET x = ID EQUAL x1 = expr RANGE_DOTS x2 = expr RPAREN UNROLL u = INT
    LBRACK c = cmd RBRACK
    { CFor (x, x1, x2, u, c) }
  | FOR LPAREN LET x = ID EQUAL x1 = expr RANGE_DOTS x2 = expr RPAREN
    LBRACK c = cmd RBRACK
    { CFor (x, x1, x2, 1, c) } ;
  | c1 = cmd c2 = cmd
    { CSeq (c1, c2) }

expr:
  | x = ID; a = access;
    { EAA (x, a) }
  | x = ID; LBRACK; idx1 = expr; RBRACK; LSQUARE; idx2 = expr; RSQUARE
    { EBankedAA (x, idx1, idx2) }
  | LPAREN e = expr RPAREN
    { e }
  | e1 = expr; bop = binop; e2 = expr
    { EBinop (bop, e1, e2) } ;
  | x = INT
    { EInt (x, true) }
  | f = FLOAT
    { EFloat f }
  | TRUE
    { EBool true }
  | FALSE
    { EBool false }
  | x = ID
    { EVar x }

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


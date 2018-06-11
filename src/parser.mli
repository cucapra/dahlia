
(* The type of tokens. *)

type token = 
  | UNROLL
  | TRUE
  | TIMES
  | SEMICOLON
  | RSQUARE
  | RPAREN
  | REASSIGN
  | RBRACK
  | RANGE_DOTS
  | PLUS
  | OR
  | NEQ
  | MINUS
  | LT
  | LSQUARE
  | LPAREN
  | LET
  | LEQ
  | LBRACK
  | INT_ANNOTATION
  | INT of (int)
  | IF
  | ID of (string)
  | GT
  | GEQ
  | FOR
  | FALSE
  | EQUAL
  | EQ
  | EOF
  | BOOL_ANNOTATION
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.command)

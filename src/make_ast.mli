open Ast

val make_assignment : id -> expression -> command

val make_int : int -> expression

val make_bool : bool -> expression

val make_binop : binop -> expression -> expression -> expression

val make_range : int -> int -> expression
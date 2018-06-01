open Ast

val make_assignment : id -> expression -> command

val make_int : int -> expression

val make_bool : bool -> expression

val make_binop : binop -> expression -> expression -> expression

val make_for : id -> expression -> expression -> command list -> command

val make_var : id -> expression

val make_array : int -> type_annotation -> expression

val make_array_update : id -> expression -> expression -> command

val make_array_access : id -> expression -> expression

val make_if : expression -> command list -> command

val make_seq : command -> command -> command
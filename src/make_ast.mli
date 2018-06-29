open Ast

val make_assignment : id -> expression -> command

val make_int : int -> expression

val make_bool : bool -> expression

val make_binop : binop -> expression -> expression -> expression

val make_for : id -> expression -> expression -> command -> command

val make_for_impl : id -> expression -> expression -> expression -> command -> command

val make_var : id -> expression

val make_array : int -> int -> type_node -> expression

val make_if : expression -> command -> command

val make_seq : command -> command -> command

val make_array_access_expl : id -> expression -> expression -> expression

val make_array_access_impl : id -> expression -> expression

val make_reassignment : expression -> expression -> command

val make_function : id -> (id * type_node) list -> command -> command

val make_app : id -> expression list -> command

val make_typedef : id -> type_node -> command

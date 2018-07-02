open Ast

type context
type delta

exception TypeError of string

val check_expr : expression -> (context * delta) -> type_node * (context * delta)

val check_cmd : command -> (context * delta) -> (context * delta)

val empty_context : context

val empty_delta : delta

val type_of_id : id -> context -> type_node

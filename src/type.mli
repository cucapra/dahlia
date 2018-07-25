open Ast
open Context

exception TypeError of string

val check_expr : expression -> (gamma * delta) -> type_node * (gamma * delta)

val check_cmd : command -> (gamma * delta) -> (gamma * delta)

val type_of_id : id -> gamma -> type_node

val type_of_alias_id : id -> delta -> type_node

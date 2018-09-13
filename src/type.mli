open Ast
open Context

exception TypeError of string

val check_expr : expr -> (gamma * delta) -> type_node * (gamma * delta)

val check_cmd : command -> (gamma * delta) -> (gamma * delta)

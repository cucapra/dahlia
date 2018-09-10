open Ast
open Context

exception TypeError of string

val check_expr : expression -> (gamma * delta) -> type_node * (gamma * delta)

val check_cmd : command -> (gamma * delta) -> (gamma * delta)

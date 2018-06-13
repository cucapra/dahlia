open Ast

type context

exception TypeError of string

val check_expr : expression -> context -> type_node * context

val check_cmd : command -> context -> context

val empty_context : context

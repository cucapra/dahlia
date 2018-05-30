open Ast

type context

exception TypeError of string

val check_expr : expression -> context -> type_node

val check_cmds : command list -> context -> context

val empty_context : context
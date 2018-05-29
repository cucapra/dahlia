open Ast

type context
type type_node

val check_expr : expression -> context -> type_node

val check_cmd : command -> context -> context
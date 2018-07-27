open Ast
open Context

exception IllegalOperation

val type_of_op : type_node -> type_node -> delta -> binop -> type_node

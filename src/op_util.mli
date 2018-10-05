open Ast

exception IllegalOperation

val type_of_op : type_node -> type_node -> binop -> type_node

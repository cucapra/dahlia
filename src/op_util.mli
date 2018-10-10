open Ast

exception IllegalOperation

val type_of_op : type_node -> type_node -> binop -> type_node

val min_int32 : int

val max_int32 : int

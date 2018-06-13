open Ast

val type_map : (id -> type_node) ref

val set_type_map : (id -> type_node) ref -> unit

val generate_c : command -> string

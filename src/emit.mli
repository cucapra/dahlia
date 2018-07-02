open Ast

val type_map : (id -> type_node) ref

val set_type_map : (id -> type_node) -> unit

val set_delta_map : (id -> type_node) -> unit

val generate_c : command -> string

open Ast

(* [(AlreadyConsumed i)] represents that an array access was performed
   using an index [i] that has already been consumed. *)
exception AlreadyConsumed

(* [(NoBinding id)] represents that the binding for [id] is missing in gamma. *)
exception NoBinding of id

(* [gamma] is a type for a typing context that binds IDs to types. *)
type gamma

val empty_gamma : gamma

(* [add_binding id t g] is a new gamma [g'] with type_node [t] bound to [id]. *)
val add_binding : id -> type_node -> gamma -> gamma

(* [get_binding id g] is [t] that's bound to [id] in gamma [g],
   if such a binding exists; else, raises [NoBinding]. *)
val get_binding : id -> gamma -> type_node

(* [consume_aa id n g] is [g'] where [g'] is a gamma with the
   index [n] of array with id [id] is consumed. Raises [AlreadyConsumed] if
   the [n] is already consumed. *)
val consume_binding : id -> gamma -> gamma

val renew_all_bindings : gamma -> gamma

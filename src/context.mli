open Ast

(* [(AlreadyConsumed i)] represents that an array access was performed
   using an index [i] that has already been consumed. *)
exception AlreadyConsumed of int

(* [(NoBinding id)] represents that the binding for [id] is missing in gamma. *)
exception NoBinding of id

(* [gamma] is a type for a typing context that binds IDs to types. *)
type gamma

(* [delta] is a type for a mapping between alias IDs and types. *)
type delta

val empty_gamma : gamma
val empty_delta : delta

(* [add_binding id t g] is a new gamma [g'] with type_node [t] bound to [id]. *)
val add_binding : id -> type_node -> gamma -> gamma

(* [add_alias_binding id t d] is a new delta [d'] with type_node [t] bound to [id]. *)
val add_alias_binding : id -> type_node -> delta -> delta

(* [get_binding id g] is [t] that's bound to [id] in gamma [g],
   if such a binding exists; else, raises [NoBinding]. *)
val get_binding : id -> gamma -> type_node

(* [get_alias_binding id d] is [t] that's bound to [id] in delta [d],
   if such a binding exists; else, raises [NoBinding]. *)
val get_alias_binding : id -> delta -> type_node

(* [consume_aa id n g] is [g'] where [g'] is a gamma with the
   index [n] of array with id [id] is consumed. Raises [AlreadyConsumed] if
   the [n] is already consumed. *)
val consume_aa : id -> int -> gamma -> gamma

(* [consume_aa_lst id [i..k] g] is [g'] where [g'] is a gamma with
   the indices i..k of array [id] consumed. Raises [AlreadyConsumed] if
   an element in [i..k] is already consumed. *)
val consume_aa_lst : id -> int list -> gamma -> gamma

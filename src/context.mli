open Ast

(* [(AlreadyConsumed i)] represents that an array access was performed
   using an index [i] that has already been consumed. *)
exception AlreadyConsumed of int
exception NoBinding

(* [gamma] is a type for a typing context that binds IDs to types. *)
type gamma

(* [delta] is a type for a mapping between alias IDs and types. *)
type delta

val empty_gamma : gamma
val empty_delta : delta

(* [bind_type id t g] is a new gamma [g'] with type_node [t] bound to [id]. *)
val add_binding : id -> type_node -> gamma -> gamma

(* [retrieve_type id g] is [Some t] that's bound to [id] in gamma [g],
   if such a binding exists; else, it's [None]. *)
val get_binding : id -> gamma -> type_node

(* [remove_binding id g] is [g'] where [g'] is [g] but without a binding to
   [id]; if [id] is not bound in [g], [g]=[g']. *)
val rem_binding : id -> gamma -> gamma

(* [consume_aa id n g] is [g'] where [g'] is a gamma with the
   index [n] of array with id [id] is consumed. Raises [AlreadyConsumed] if
   the [n] is already consumed. *)
val consume_aa : id -> int -> gamma -> gamma

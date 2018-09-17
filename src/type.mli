open Ast
open Context

exception TypeError of string

(** Typechecks the command and returns the final type environment. *)
val typecheck : command -> gamma

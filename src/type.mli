open Ast
open Context

(** Typechecks the command and returns the final type environment. *)
val typecheck : command -> gamma

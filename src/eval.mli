
(* Acknowledgements:
    - Cornell's CS 3110 Fall 2018 A4
    - Cornell's CS 6110 Spring 2018 Lecture 8 notes *)

open Ast

exception Unimplemented

type env

(* [empty_env] is an environment with no bindings. *)
val empty_env : env

(* [eval_expression (exp, env)] is (v, env') where v is the 
   value that results from evaluating [exp] with environment [env], 
   and [env'] is the environment that results from computing [v]. *)
val eval_expression : expression * env -> value * env

(* [eval_command (cmd, env)] is [env] where [env] is the environment
   that results after evaluating [cmd]. *)
val eval_command : command * env -> env

(* [string_of_env env] is a string representation of [env]. *)
val string_of_env : env -> string

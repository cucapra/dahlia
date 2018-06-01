
(* Acknowledgements:
    - Cornell's CS 3110 Fall 2018 A4
    - Cornell's CS 6110 Spring 2018 Lecture 8 notes *)

open Ast

exception Unimplemented

type env

val initial_env : env

val eval_expression : expression * env -> value * env

val eval_command : command * env -> env

val string_of_env : env -> string

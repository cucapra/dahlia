
(* Acknowledgements:
    - Cornell's CS 3110 Fall 2018 A4
    - Cornell's CS 6110 Spring 2018 Lecture 8 notes *)

open Ast

exception Unimplemented

module EnvMap = Map.Make(
  struct
    type t = id
    let compare = String.compare
  end
)

type env = value EnvMap.t

let initial_env = EnvMap.empty

let rec eval_binop binop e1 e2 = failwith "Implement me"

let rec eval_expression : expression * env -> value * env = fun (exp, e) ->
  let open EnvMap in
  match exp with
  | EInt x -> VInt x, e
  | EVar x -> ((find x e), e)
  | EBool x -> VBool x, e
  | EBinop (binop, e1, e2) -> eval_binop binop e1 e2, e
  | ERange (x1, x2) -> VRange (x1, x2), e

let rec eval_command : command * env -> env = fun (cmd, e) ->
  let open EnvMap in
  match cmd with
  | CAssignment (x, exp) -> 
    let v, env = eval_expression (exp, e) in
    add x v env

let string_of_val = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VRange (i1, i2) -> (string_of_int i1) ^ ".." ^ (string_of_int i2)

let string_of_env env =
  let produce_string = (fun id v acc ->
    id ^ ": " ^ (string_of_val v) ^ "\n" ^ acc) in
  String.trim (EnvMap.fold produce_string env "")


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

let expr_of_int i = EInt i

let int_of_val = function
  | VInt i -> i
  | _ -> failwith "Implement type checking"

let val_of_int i = VInt i
let val_of_bool b = VBool b

let rec eval_for x x1 x2 cmds env i = 
  if i <= x2 then
    let env_with_x = eval_command ((CAssignment (x, (expr_of_int i)), env)) in
    eval_for x x1 x2 cmds (eval_cmd_list cmds env_with_x) (i+1)
  else
    env

and eval_binop binop e1 e2 env =
  let v1, env1 = eval_expression (e1, env) in
  let v2, env2 = eval_expression (e2, env1) in
  match binop with
  | BopPlus -> 
    val_of_int ((int_of_val v1) + (int_of_val v2)), env2
  | BopGeq ->
    val_of_bool ((int_of_val v1) >= (int_of_val v2)), env2
  | BopLeq ->
    val_of_bool ((int_of_val v1) <= (int_of_val v2)), env2
  | BopGt ->
    val_of_bool ((int_of_val v1) > (int_of_val v2)), env2
  | BopLt ->
    val_of_bool ((int_of_val v1) < (int_of_val v2)), env2
  | _ -> failwith "Implement other binops"

and eval_expression : expression * env -> value * env = fun (exp, e) ->
  let open EnvMap in
  match exp with
  | EInt x -> VInt x, e
  | EVar x -> ((find x e), e)
  | EBool x -> VBool x, e
  | EBinop (binop, e1, e2) -> eval_binop binop e1 e2 e
  | EArray arr -> (VArray (Array.map (fun elem -> let v, _ = eval_expression (elem, e) in v) arr)), e

and eval_cmd_list cmds env =
  match cmds with
  | h::t -> eval_cmd_list t (eval_command (h, env))
  | [] -> env

and eval_array_update x i expression env =
  let arr, _ = eval_expression (EVar x, env) in
  let index, _ = eval_expression (i, env) in
  let new_arr_val, _ = eval_expression (expression, env) in
  match arr, index with
  | VArray arr, VInt i -> Array.set arr i new_arr_val; env
  | _ -> failwith "Illegal := applied" (* FIXME: refactor this *)

and eval_bool b =
  match b with
  | VBool b -> b
  | _ -> failwith "Illegal bool operator applied" (* FIXME: refactor this *)

and eval_command : command * env -> env = fun (cmd, e) ->
  let open EnvMap in
  match cmd with
  | CAssignment (x, exp) -> 
    let v, env = eval_expression (exp, e) in
    add x v env
  | CFor (x, x1, x2, cmds) -> 
    let i1, _ = eval_expression (x1, e) in
    let i2, _ = eval_expression (x2, e) in
    begin
      match i1, i2 with
      | VInt v1, VInt v2 -> eval_for x v1 v2 cmds e 0
      | _ -> failwith "Undefined" (* FIXME: refactor this *)
    end
  | CArrayUpdate (x, index, expression) -> eval_array_update x index expression e
  | CIf (b, body) -> 
    let truth_value, _ = eval_expression (b, e) in
    if eval_bool truth_value then (eval_cmd_list body e) else e
    

let rec string_of_val = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VRange (i1, i2) -> (string_of_int i1) ^ ".." ^ (string_of_int i2)
  | VArray arr -> 
    let accum = (fun acc elem -> acc ^ ", " ^ (string_of_val elem)) in
    let unprettified = Array.fold_left accum "" arr in
    String.sub unprettified 1 ((String.length unprettified) - 1)

let string_of_env env =
  let produce_string = (fun id v acc ->
    id ^ ": " ^ (string_of_val v) ^ "\n" ^ acc) in
  String.trim (EnvMap.fold produce_string env "")

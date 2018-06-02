
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

let empty_env = EnvMap.empty

let expr_of_int i = EInt i

let int_of_val = function
  | VInt i -> i
  | _ -> failwith "Implement type checking"

let val_of_int i = VInt i
let val_of_bool b = VBool b

let rec eval_binop binop e1 e2 env =
  let v1, env1 = eval_expression (e1, env) in
  let v2, env2 = eval_expression (e2, env1) in
  match binop with
  | BopPlus -> 
    val_of_int ((int_of_val v1) + (int_of_val v2)), env2
  | BopGeq ->
    val_of_bool ((int_of_val v1) >= (int_of_val v2)), env2
  | BopLeq ->
    val_of_bool ((int_of_val v1) <= (int_of_val v2)), env2
  | BopNeq ->
    val_of_bool ((int_of_val v1) != (int_of_val v2)), env2
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
  | EArrayAccess (id, index) ->
    let VArray arr, _ = eval_expression (EVar id, e) in
    let VInt i, _ = eval_expression (index, e) in (* FIXME: consider other cases *)
    Array.get arr i, e


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
  | _ -> failwith "Type checker failed to catch bool op applied to non-bool"

and eval_assignment x exp env =
  eval_expression (exp, env) 
  |> (fun (v, env') -> EnvMap.add x v env)

and eval_for_body x i b cmd env =
  if i <= b then
    eval_command (CAssignment (x, EInt i), env)
    |> (fun env' -> eval_command (cmd, env'))
    |> (fun env'' -> eval_for_body x (i+1) b cmd env'')
  else env

and eval_for x a b cmd env =
  eval_expression (a, env) |> (fun (i1, env1) ->
  eval_expression (b, env1) |> (fun (i2, env2) ->
  match i1, i2 with
  | VInt v1, VInt v2 -> eval_for_body x v1 v2 cmd env
  | _ -> failwith "Type checker failed to catch non-ints in for-loop range"))

and eval_command : command * env -> env = fun (cmd, e) ->
  let open EnvMap in
  match cmd with
  | CAssignment (x, exp) -> eval_assignment x exp e
  | CFor (x, a, b, body) -> eval_for x a b body e
  | CArrayUpdate (x, index, expression) -> eval_array_update x index expression e
  | CIf (b, body) -> 
    let truth_value, _ = eval_expression (b, e) in
    if eval_bool truth_value then eval_command (body, e) else e
  | CSeq (c1, c2) ->
    let new_env = eval_command (c1, e) in
    eval_command (c2, new_env)

let rec string_of_arr arr =
  (fun acc elem -> acc ^ ", " ^ (string_of_val elem))
  |> (fun f -> Array.fold_left f "" arr)
  |> (fun s -> String.sub s 1 ((String.length s) - 1))

and string_of_val = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VRange (i1, i2) -> (string_of_int i1) ^ ".." ^ (string_of_int i2)
  | VArray arr -> string_of_arr arr

let string_of_env env =
  (fun id v acc -> id ^ ": " ^ (string_of_val v) ^ "\n" ^ acc)
  |> (fun f -> String.trim (EnvMap.fold f env ""))

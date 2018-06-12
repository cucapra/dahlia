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

let rec eval_expression : expression * env -> value * env = fun (exp, env) ->
  let open EnvMap in
  match exp with
  | EInt x                   -> eval_int (x, env)
  | EBool b                  -> eval_bool (b, env)
  | EVar id                  -> eval_var (id, env)
  | EBinop (binop, e1, e2)   -> eval_binop (binop, e1, e2, env)
  | EArray (_, _, a)          -> eval_array (a, env)
  | EArrayAccess (id, index) -> eval_array_access (id, index, env)

and eval_int (i, env) = VInt i, env
and eval_bool (b, env) = VBool b, env
and eval_var (x, env) = EnvMap.find x env, env

(* FIXME: the environment is not affected by evaluating each element
   of the array, need to find out if this is necessary *)
and eval_array (a, env) =
  (fun exp -> eval_expression (exp, env) |> fun (v, _) -> v)
  |> fun f -> VArray (Array.map f a), env

and eval_array_access (id, index, env) =
  eval_var (id, env)            |> fun (arr_val, env') -> 
  eval_expression (index, env') |> fun (index_val, env'') ->
  match arr_val, index_val with
  | VArray arr, VInt i -> Array.get arr i, env''
  | _ -> failwith "Type checker failed to catch non-int idx or non-array access"

and eval_binop (binop, e1, e2, env) =
  match binop, eval_expression (e1, env), eval_expression (e2, env) with
  | BopPlus,  (VInt i1, env1), (VInt i2, env2)   -> VInt (i1 + i2),   env
  | BopMinus, (VInt i1, env1), (VInt i2, env2)   -> VInt (i1 - i2),   env
  | BopTimes, (VInt i1, env1), (VInt i2, env2)   -> VInt (i1 * i2),   env
  | BopGt,    (VInt i1, env1), (VInt i2, env2)   -> VBool (i1 > i2),  env
  | BopLt,    (VInt i1, env1), (VInt i2, env2)   -> VBool (i1 < i2),  env
  | BopEq,    (VInt i1, env1), (VInt i2, env2)   -> VBool (i1 = i2),  env
  | BopGeq,   (VInt i1, env1), (VInt i2, env2)   -> VBool (i1 >= i2), env
  | BopLeq,   (VInt i1, env1), (VInt i2, env2)   -> VBool (i1 <= i2), env
  | BopNeq,   (VInt i1, env1), (VInt i2, env2)   -> VBool (i1 != i2), env
  | BopAnd,   (VBool b1, env1), (VBool b2, env2) -> VBool (b1 && b2), env
  | BopOr,    (VBool b1, env1), (VBool b2, env2) -> VBool (b1 || b2), env
  | _ -> failwith "Type checker failed to catch illegal operation application "

let rec eval_command : command * env -> env = fun (cmd, env) ->
  match cmd with
  | CAssignment (x, exp)         -> eval_assignment x exp env
  | CIf (b, body)                -> eval_if b body env
  | CSeq (c1, c2)                -> eval_seq c1 c2 env
  | CFor (x, a, b, body)         -> eval_for x a b body env
  | CReassign _ -> failwith "Implement me"

and eval_assignment x exp env =
  eval_expression (exp, env) 
  |> fun (v, env') -> EnvMap.add x v env

and eval_if cond body env =
  eval_expression (cond, env) |> fun (v_cond, env') ->
  match v_cond with
  | VBool b -> if b then eval_command (body, env') else env'
  | _ -> failwith "Type checker failed to catch non-bool in if-condition"

and eval_seq c1 c2 env =
  eval_command (c1, env) |> fun env' -> eval_command (c2, env')

and eval_for x a b cmd env =
  eval_expression (a, env)  |> fun (i1, env1) ->
  eval_expression (b, env1) |> fun (i2, env2) ->
  match i1, i2 with
  | VInt v1, VInt v2 -> eval_for_body x v1 v2 cmd env
  | _ -> failwith "Type checker failed to catch non-ints in for-loop range"

and eval_for_body x i b cmd env =
  if i <= b then
    eval_command (CAssignment (x, EInt i), env)
    |> fun env' -> eval_command (cmd, env')
    |> fun env'' -> eval_for_body x (i+1) b cmd env''
  else env

and eval_array_update x i exp env =
  eval_var (x, env)            |> fun (arr, env') -> 
  eval_expression (i, env')    |> fun (idx_val, env'') ->
  eval_expression (exp, env'') |> fun (v, env''') ->
  match arr, idx_val with
  | VArray arr, VInt i -> Array.set arr i v; env'''
  | _ -> failwith "Type checker failed to catch illegal update operation"

let rec string_of_val = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VRange (i1, i2) -> (string_of_int i1) ^ ".." ^ (string_of_int i2)
  | VArray arr -> string_of_arr arr

and string_of_arr arr =
  (fun acc elem -> acc ^ ", " ^ (string_of_val elem))
  |> (fun f -> Array.fold_left f "" arr)
  |> (fun s -> String.sub s 1 ((String.length s) - 1))

let string_of_env env =
  (fun id v acc -> id ^ ": " ^ (string_of_val v) ^ "\n" ^ acc)
  |> fun f -> String.trim (EnvMap.fold f env "")

let stringified_binding id env =
  EnvMap.find id env
  |> string_of_val


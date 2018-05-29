open Ast

exception TypeError of string

module ContextMap = Map.Make(
  struct
    type t = id
    let compare = String.compare
  end
)

type type_node =
  | TInt
  | TBool

type context = type_node ContextMap.t

let allow_ints = function
  | BopEq
  | BopNeq
  | BopGeq 
  | BopLeq
  | BopLt
  | BopGt
  | BopPlus
  | BopMinus
  | BopTimes -> true

let check_binop binop e1 e2 =
  match e1, e2 with
  | TInt, TInt ->
    if allow_ints binop then
      TInt 
    else 
      raise (TypeError "Cannot apply binary operation to pair of ints")
  | _ -> failwith "Implement rest of types"

let rec check_expr exp context =
  match exp with
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EVar x -> ContextMap.find x context
  | EBinop (binop, e1, e2) -> 
    check_binop binop (check_expr e1 context) (check_expr e2 context)

let rec check_cmds cmds context =
  match cmds with
  | h::t ->
    check_cmds t (check_cmd h context)
  | [] -> context
  
and check_cmd cmd context =
  let open ContextMap in
  match cmd with
  | CAssignment (x, e1) -> add x (check_expr e1 context) context
  | CFor (x, r1, r2, cmds) -> (* FIXME: for loops only support iterating with ints *)
    check_cmds cmds (add x TInt context) 
open Ast

exception TypeError of string

module ContextMap = Map.Make(
  struct
    type t = id
    let compare = String.compare
  end
)

type context = type_node ContextMap.t

let empty_context = ContextMap.empty

let string_of_binop = function
  | BopEq -> "="
  | BopNeq -> "!="
  | BopGeq -> ">="
  | BopLeq -> "<="
  | BopLt -> "<"
  | BopGt -> ">"
  | BopPlus -> "+"
  | BopMinus -> "-"
  | BopTimes -> "*"

let string_of_type = function
  | TBool -> "bool"
  | TInt -> "int"

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

let check_binop binop t1 t2 =
  match t1, t2 with
  | TInt, TInt ->
    if allow_ints binop then
      TInt 
    else 
      raise (TypeError "Cannot apply binary operation to pair of ints")
  | _ -> 
    raise (TypeError 
      ("Can't apply operator '" ^ (string_of_binop binop) ^ 
       "' to " ^ (string_of_type t1) ^ " and " ^ (string_of_type t2)))

let rec check_expr exp context =
  match exp with
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EVar x -> ContextMap.find x context
  | EBinop (binop, e1, e2) -> 
    check_binop binop (check_expr e1 context) (check_expr e2 context)
  | EArray arr -> TArray

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
  | CArrayUpdate (x, index, exp) -> context (* FIXME *)
  | CIf _ -> context (* FIXME *)
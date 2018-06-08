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
  | BopOr -> "||"
  | BopAnd -> "&&"

let rec string_of_type = function
  | TBool -> "bool"
  | TInt -> "int"
  | TArray t -> (string_of_type t) ^ " array"

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
  | _ -> false

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
  | EArray _ -> raise (TypeError "Can't refer to array literal")
  | EArrayAccess _ -> TArray (TInt) (* FIXME *)

let array_elem_error arr_id arr_t elem_t =
  ("Array " ^ arr_id ^ "is of type " ^ (string_of_type arr_t) ^ 
  "; tried to add element of type " ^ (string_of_type elem_t))

let arr_idx_type_error =
  "Tried to index into array with non-integer index"

let arr_type_error =
  "Tried to index into non-array"

let cond_type_error =
  "Non-boolean conditional"

let rec check_cmd cmd context =
  match cmd with
  | CSeq (c1, c2) -> check_seq c1 c2 context
  | CAssignment (x, e1) -> check_assignment x e1 context
  | CFor (x, r1, r2, body) -> check_for x r1 r2 body context
  | CArrayUpdate (arr_id, idx, exp) -> check_array_update arr_id idx exp context
  | CIf (cond, cmd) -> check_if cond cmd context

and check_seq c1 c2 context =
  check_cmd c1 context
  |> fun context' -> check_cmd c2 context'

and check_assignment id exp context =
  match exp with
  | EArray (t, _) -> ContextMap.add id (TArray t) context
  | other_exp -> ContextMap.add id (check_expr other_exp context) context

and check_for id r1 r2 body context =
  check_expr r1 context |> fun r1_type ->
  check_expr r2 context |> fun r2_type ->
  match r1_type, r2_type with
  | TInt, TInt -> check_cmd body (ContextMap.add id TInt context)
  | _ -> raise (TypeError "Range start/end must be integers")



and check_array_update id index exp context =
  ContextMap.find id context |> fun array_type ->
  check_expr index context   |> fun index_type ->
  check_expr exp context     |> fun expr_type  ->
  match array_type, index_type with
  | TArray t, TInt ->
    if t = expr_type then context
    else raise (TypeError (array_elem_error id t expr_type))
  | TArray _, _ ->
    raise (TypeError arr_idx_type_error)
  | _, _ ->
    raise (TypeError arr_type_error)
 
and check_if cond cmd context =
  match check_expr cond context with
  | TBool -> check_cmd cmd context
  | _ -> raise (TypeError cond_type_error)

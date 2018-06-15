open Ast

exception TypeError of string

let compare_opts i1 i2 =
  match i1, i2 with
  | None, None -> 0
  | Some i1, None -> 1
  | None, Some i2 -> -1
  | Some i1, Some i2 ->
    if i1>i2 then 1
    else if i1<i2 then -1
    else 0

(* FIXME: use a HashTbl*)
module ContextMap = Map.Make(
  struct
    type t = id * int option
    let compare (ida, i1) (idb, i2) =
      if (String.compare ida idb) != 0 then String.compare ida idb
      else compare_opts i1 i2
  end
)

type context = type_node ContextMap.t

let empty_context = ContextMap.empty

let global_context = ref ContextMap.empty
let rec type_map = ref (fun id -> ContextMap.find (id, None) !global_context)

and string_of_type = function
  | TBool -> "bool"
  | TInt _ -> "int"
  | TArray t -> (string_of_type t) ^ " array"

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

let allow_bools = function
  | BopAnd
  | BopOr -> true
  | _ -> false

let bop_type = function
  | BopEq -> TBool
  | BopNeq -> TBool
  | BopGeq -> TBool
  | BopLeq -> TBool
  | BopLt -> TBool
  | BopGt -> TBool
  | BopPlus -> TInt false
  | BopMinus -> TInt false
  | BopTimes -> TInt false
  | BopAnd -> TBool
  | BopOr -> TBool

(* FIXME: refactor/change this *)
let is_int_literal = function
  | EInt _ -> true
  | _ -> false

let check_binop binop (t1, c) (t2, c) =
  match t1, t2 with
  | TInt _, TInt _ ->
    if allow_ints binop then
      bop_type binop, c
    else 
      raise (TypeError "Cannot apply binary operation to pair of ints")
  | TBool, TBool ->
    if allow_bools binop then
      bop_type binop, c
     else
      raise (TypeError "Cannot apply binary operation to pair of bools")
  | _ -> 
    raise (TypeError 
      ("Can't apply operator '" ^ (string_of_binop binop) ^ 
       "' to " ^ (string_of_type t1) ^ " and " ^ (string_of_type t2)))

let rec check_expr exp context =
  match exp with
  | EInt (_, s)            -> TInt s, context
  | EBool _                -> TBool, context
  | EVar x                 -> ContextMap.find (x, None) context, context
  | EBinop (binop, e1, e2) -> 
    check_binop binop (check_expr e1 context) (check_expr e2 context)
  | EArray _               -> raise (TypeError "Can't refer to array literal")
  | EArrayAccess (id, _)   -> check_array_access id context
  | _ -> failwith "Implement rest of expressions"

and check_array_access_explicit id idx1 idx2 context =
  check_expr idx1 context |> fun (idx1_t, c') ->
  check_expr idx2 c'      |> fun (idx2_t, c'') ->
  match idx1_t, idx2_t with
  | TInt s1, TInt _ -> 
    if s1 then
    begin
      match idx1 with
      | EInt (idx, _) -> 
        if ContextMap.mem (id, Some idx) c'' then
          TInt s1, ContextMap.remove (id, Some idx) c''
        else raise (TypeError ("Illegal bank access: " ^ (string_of_int idx)))
    end
    else raise (TypeError "Bank accessor must be static") 
  | _ -> raise (TypeError "Array indices must be integers")

and check_array_access id context =
  try
    match check_expr (EVar id) context with
    | (TArray t, c) -> t, c (* FIXME: fix so affine works with transp... ContextMap.remove id c *)
    | _ -> raise (TypeError "Tried to index into non-array")
  with
    Not_found -> raise (TypeError "Tried to access array illegal number of times")

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
  (match cmd with
  | CReassign (target, exp) -> check_reassign target exp context
  | CSeq (c1, c2)           -> check_seq c1 c2 context
  | CAssignment (x, e1)     -> check_assignment x e1 context
  | CFor (x, r1, r2, body)  -> check_for x r1 r2 body context
  | CIf (cond, cmd)         -> check_if cond cmd context)
  |> fun c -> global_context := c; c

and check_reassign target exp context =
  match target, exp with
  | EArrayAccess (id, _), expr -> 
    check_array_access id context |> fun (t_arr, c')  ->
    check_expr exp c'             |> fun (t_exp, c'') ->
    if t_arr=t_exp then c'' else raise (TypeError "Tried to populate array with incorrect type")
  | EArrayExplAccess (id, idx1, idx2), expr ->
    begin
    check_array_access_explicit id idx1 idx2 context |> fun (t_arr, c) ->
    check_expr exp c                                 |> fun (t_exp, c') ->
    match t_arr, t_exp with
    | TInt _, TInt _ -> c'
    | t1, t2 -> if t1=t2 then c' else raise (TypeError "Tried to populate array with incorrect type")
    end
  | EVar id, expr -> context
  | _ -> raise (TypeError "Used reassign operator on illegal types")

and check_seq c1 c2 context =
  check_cmd c1 context
  |> fun context' -> check_cmd c2 context'

and add_array_banks id bank_num context t i =
  if i=bank_num then context
  else add_array_banks id bank_num (ContextMap.add (id, Some i) (TArray t) context) t (i+1)

and check_assignment id exp context =
  match exp with
  | EArray (t, b, _) -> 
    (* For each element of the array, add a copy to the context *)
    add_array_banks id b context t 0
  | other_exp -> check_expr other_exp context |> fun (t, c) ->
    ContextMap.add (id, None) t c

and check_for id r1 r2 body context =
  check_expr r1 context |> fun (r1_type, c') ->
  check_expr r2 c'      |> fun (r2_type, c'') ->
  match r1_type, r2_type with
  | TInt _, TInt _ -> check_cmd body (ContextMap.add (id, None) (TInt false) c'')
  | _ -> raise (TypeError "Range start/end must be integers")

and check_if cond cmd context =
  match check_expr cond context with
  | TBool, c -> check_cmd cmd c
  | t -> raise (TypeError cond_type_error)

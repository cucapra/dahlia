open Ast

exception TypeError of string

type context = ((id * int option), type_node) Hashtbl.t
type delta   = (id, type_node) Hashtbl.t

let empty_context = Hashtbl.create 100
let empty_delta   = Hashtbl.create 100

let type_of_id id context = Hashtbl.find context (id, None)

let type_of_alias_id id delta = Hashtbl.find delta id

let rec string_of_type = function
  | TBool -> "bool"
  | TInt _ -> "int"
  | TArray (t, _, _) -> (string_of_type t) ^ " array"
  | TIndex _ -> "index"
  | TAlias id -> id
  | TFloat -> "float"

let string_of_binop = function
  | BopEq    -> "="
  | BopNeq   -> "!="
  | BopGeq   -> ">="
  | BopLeq   -> "<="
  | BopLt    -> "<"
  | BopGt    -> ">"
  | BopPlus  -> "+"
  | BopMinus -> "-"
  | BopTimes -> "*"
  | BopOr    -> "||"
  | BopAnd   -> "&&"

let bop_type a b op =
  match a, b, op with
  | _, _, BopEq                        -> TBool
  | _, _, BopNeq                       -> TBool
  | _, _, BopGeq                       -> TBool
  | _, _, BopLeq                       -> TBool
  | _, _, BopLt                        -> TBool
  | _, _, BopGt                        -> TBool
  | (TInt _), (TInt _), BopPlus        -> TInt None
  | (TInt _), TFloat, BopPlus          -> TFloat
  | TFloat, (TInt _), BopPlus          -> TFloat
  | TFloat, TFloat, BopPlus            -> TFloat
  | TFloat, (TInt _), BopMinus         -> TFloat
  | (TInt _), (TInt _), BopMinus       -> TInt None
  | (TInt _), TFloat, BopMinus         -> TFloat
  | TFloat, TFloat, BopMinus           -> TFloat
  | TFloat, (TInt _), BopTimes         -> TFloat
  | (TInt _), (TInt _), BopTimes       -> TInt None
  | (TInt _), TFloat, BopTimes         -> TFloat
  | TFloat, TFloat, BopTimes           -> TFloat
  | _, _, BopAnd                       -> TBool
  | _, _, BopOr                        -> TBool
  | (TIndex i), TInt _, BopPlus        -> TIndex i
  | TInt _, (TIndex i), BopPlus        -> TIndex i
  | (TIndex i), TInt _, BopMinus       -> TIndex i
  | TInt _, (TIndex i), BopMinus       -> TIndex i
  | (TIndex i), TInt _, BopTimes       -> TIndex i
  | TInt _, (TIndex i), BopTimes       -> TIndex i
  | (TIndex i1), (TIndex i2), BopPlus  -> TIndex i2 (*FIXME*)
  | (TIndex i1), (TIndex i2), BopMinus -> TIndex i2 (*FIXME*)
  | (TIndex i1), (TIndex i2), BopTimes -> TIndex i2 (*FIXME*)

(* FIXME: refactor this! *)
let rec is_int delta = function
  | TInt _ -> true
  | TAlias t -> is_int delta (Hashtbl.find delta t)
  | _ -> false

let rec is_bool delta = function
  | TBool -> true
  | TAlias t -> is_bool delta (Hashtbl.find delta t)
  | _ -> false

let rec is_float delta = function
  | TFloat -> true
  | TAlias t -> is_float delta (Hashtbl.find delta t)
  | _ -> false

let rec is_index delta = function
  | TIndex _ -> true
  | TAlias t -> is_index delta (Hashtbl.find delta t)
  | _ -> false

let rec types_equal delta t1 t2 =
  match t1, t2 with
  | TInt _, TInt _ -> true
  | TArray (a1, bf1, s1), TArray (a2, bf2, s2) -> bf1=bf2 && s1=s2 && types_equal delta a1 a2
  | TIndex _, TIndex _ -> true
  | TAlias t1, t2 -> types_equal delta (Hashtbl.find delta t1) t2
  | t1, TAlias t2 -> types_equal delta t1 (Hashtbl.find delta t2)
  | t1, t2 -> t1=t2

let legal_op t1 t2 delta = function
  | BopEq    -> (is_int delta t1 && is_int delta t2)
                || (is_float delta t1 && is_float delta t2)
                || (is_index delta t1 && is_int delta t2)
                || (is_int delta t1 && is_index delta t2)
                || (is_index delta t1 && is_index delta t2)
  | BopNeq   -> is_int delta t1 && is_int delta t2
                || (is_float delta t1 && is_float delta t2)
                || (is_index delta t1 && is_int delta t2)
                || (is_int delta t1 && is_index delta t2)
                || (is_index delta t1 && is_index delta t2)
  | BopGeq   -> is_int delta t1 && is_int delta t2
                || (is_float delta t1 && is_float delta t2)
                || (is_index delta t1 && is_int delta t2)
                || (is_int delta t1 && is_index delta t2)
                || (is_index delta t1 && is_index delta t2)
  | BopLeq   -> is_int delta t1 && is_int delta t2
                || (is_float delta t1 && is_float delta t2)
                || (is_index delta t1 && is_int delta t2)
                || (is_int delta t1 && is_index delta t2)
                || (is_index delta t1 && is_index delta t2)
  | BopLt    -> is_int delta t1 && is_int delta t2
                || (is_float delta t1 && is_float delta t2)
                || (is_index delta t1 && is_int delta t2)
                || (is_int delta t1 && is_index delta t2)
                || (is_index delta t1 && is_index delta t2)
  | BopGt    -> is_int delta t1 && is_int delta t2
                || (is_float delta t1 && is_float delta t2)
                || (is_index delta t1 && is_int delta t2)
                || (is_int delta t1 && is_index delta t2)
                || (is_index delta t1 && is_index delta t2)
  | BopPlus  -> is_int delta t1 && is_int delta t2
                || (is_float delta t1 && is_float delta t2)
                || (is_index delta t1 && is_int delta t2)
                || (is_int delta t1 && is_index delta t2)
                || (is_index delta t1 && is_index delta t2)
  | BopMinus -> is_int delta t1 && is_int delta t2
                || (is_float delta t1 && is_float delta t2)
                || (is_index delta t1 && is_int delta t2)
                || (is_int delta t1 && is_index delta t2)
                || (is_index delta t1 && is_index delta t2)
  | BopTimes -> is_int delta t1 && is_int delta t2
                || (is_float delta t1 && is_float delta t2)
                || (is_index delta t1 && is_int delta t2)
                || (is_int delta t1 && is_index delta t2)
                || (is_index delta t1 && is_index delta t2)
  | BopAnd   -> is_bool delta t1 && is_bool delta t2
  | BopOr    -> is_bool delta t1 && is_bool delta t2

let rec check_expr exp (context, delta) =
  match exp with
  | EInt (i, s)                       -> check_int i s (context, delta)
  | EFloat f                          -> check_float f (context, delta)
  | EBool _                           -> TBool, (context, delta)
  | EVar x                            -> Hashtbl.find context (x, None), (context, delta)
  | EBinop (binop, e1, e2)            -> check_binop binop e1 e2 (context, delta)
  | EArray _                          -> raise (TypeError "Can't refer to array literal")
  | EArrayExplAccess (id, idx1, idx2) -> check_aa_expl id idx1 idx2 (context, delta)
  | EIndex idx                        -> failwith "Unreachable"
  | EArrayImplAccess (id, i)          -> check_aa_impl id i (context, delta)

and check_int i is_stat (ctx, dta) = (if is_stat then TInt (Some i) else TInt (None)), (ctx, dta)

and check_float f (ctx, dta) = TFloat, (ctx, dta)

and check_binop binop e1 e2 (c, d) =
  check_expr e1 (c, d)   |> fun (t1, (c1, d1)) ->
  check_expr e2 (c1, d1) |> fun (t2, (c2, d2)) ->
  if legal_op t1 t2 d2 binop then bop_type t1 t2 binop, (c2, d2)
  else raise (TypeError 
    ("Illegal operation: can't apply operator '" ^
     (string_of_binop binop) ^ 
     "' to " ^ 
     (string_of_type t1) ^ 
     " and " ^ 
     (string_of_type t2)))

and check_aa_expl id idx1 idx2 (c, d) =
  check_expr idx1 (c, d)   |> fun (idx1_t, (c1, d1)) ->
  check_expr idx2 (c1, d1) |> fun (idx2_t, (c2, d2)) ->
  match idx1_t, idx2_t, Hashtbl.find c (id, None) with
  | TInt (None), _, _-> raise (TypeError "Bank accessor must be static")
  | TInt (Some i), TInt _, TArray (a_t, _, _) ->
    (if Hashtbl.mem c2 (id, Some i) then
      a_t, ((Hashtbl.remove c2 (id, Some i); c2), d2)
    else raise (TypeError ("Illegal bank access: " ^ (string_of_int i))))
  | TInt (Some i), TArray _, TArray (a_t, _, _) -> 
    (if Hashtbl.mem c2 (id, Some i) then
      a_t, ((Hashtbl.remove c2 (id, Some i); c2), d2)
    else raise (TypeError ("Illegal bank access: " ^ (string_of_int i))))
  | _ -> raise (TypeError "Bank accessor must be static") 

and check_idx id idx a_t (c, d) =
  if (List.exists 
    (fun bank -> 
      try ignore (Hashtbl.find c (id, Some bank)); false
      with Not_found -> true) 
    idx)
  then raise (TypeError "Illegal bank access")
  else 
    List.iter (fun i -> Hashtbl.remove c (id, Some i)) idx;
    a_t, (c, d)
  
and check_aa_impl id i (c, d) =
  match check_expr i (c, d), Hashtbl.find c (id, None) with
  | (TIndex idxs, _), TArray (a_t, _, _) -> check_idx id idxs a_t (c, d)
  | _                                    -> raise (TypeError "Invalid array accessor")

let rec check_cmd cmd (context, delta) =
  match cmd with
  | CSeq (c1, c2)                 -> check_seq c1 c2 (context, delta)
  | CIf (cond, cmd)               -> check_if cond cmd (context, delta)
  | CFor (x, r1, r2, body)        -> check_for x r1 r2 body (context, delta)
  | CForImpl (x, r1, r2, u, body) -> check_for_impl x r1 r2 body u (context, delta)
  | CAssign (x, e1)               -> check_assignment x e1 (context, delta)
  | CReassign (target, exp)       -> check_reassign target exp (context, delta)
  | CFuncDef (id, args, body)     -> check_funcdef id args body (context, delta)
  | CApp (id, args)               -> check_app id args (context, delta)
  | CTypeDef (id, t)              -> check_typedef id t (context, delta)

and check_seq c1 c2 (context, delta) =
  check_cmd c1 (context, delta)
  |> fun (context', delta') -> check_cmd c2 (context', delta')

and check_if cond cmd (context, delta) =
  match check_expr cond (context, delta) with
  | TBool, (ctx', dta') -> check_cmd cmd (ctx', dta')
  | _ -> raise (TypeError "Non-boolean conditional")
  
and check_for id r1 r2 body (c, d) =
  check_expr r1 (c, d)   |> fun (r1_type, (c1, d1)) ->
  check_expr r2 (c1, d1) |> fun (r2_type, (c2, d2)) ->
  match r1_type, r2_type with
  | TInt _, TInt _ -> 
    Hashtbl.add c2 (id, None) (TInt None); check_cmd body (c2, d2)
  | _ -> raise (TypeError "Range start/end must be integers")

and (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

and check_for_impl id r1 r2 body u (context, delta) =
  check_expr r1 (context, delta) |> fun (r1_type, (c1, d1)) ->
  check_expr r2 (context, delta) |> fun (r2_type, (c2, d2)) ->
  match r1_type, r2_type with
  | TInt _, TInt _ ->
    Hashtbl.add c2 (id, None) (TIndex (0--(u-1)));
    check_cmd body (c2, d2)
  | _ -> raise (TypeError "Range start/end must be integers")

and add_array_banks s bf id bank_num (context, delta) t i =
  if i=bank_num then (context, delta)
  else 
    (Hashtbl.add context (id, Some i) (TArray (t, bf, s)); 
     (add_array_banks s bf id bank_num (context, delta) t (i+1)))

and check_assignment id exp (context, delta) =
  match exp with
  | EArray (t, b, a) -> 
    let s = Array.length a in
    Hashtbl.add context (id, None) (TArray (t, b, s)); 
    add_array_banks s b id b (context, delta) t 0
  | other_exp -> 
    check_expr other_exp (context, delta) |> fun (t, (c, d)) ->
    Hashtbl.add c (id, None) t; (c, d)

and check_reassign target exp (context, delta) =
  match target, exp with
  | EArrayExplAccess (id, idx1, idx2), expr ->
    check_aa_expl id idx1 idx2 (context, delta) |> fun (t_arr, (c, d)) ->
    check_expr exp (c, d)                       |> fun (t_exp, (c', d')) ->
    if types_equal delta t_arr t_exp then (c', d')
    else raise (TypeError "Tried to populate array with incorrect type")
  | EVar id, expr -> (context, delta)
  | EArrayImplAccess (id, idx), expr -> 
    let (_, (c'', d'')) = check_aa_impl id idx (context, delta) in
    (c'', d'')
  | _ -> raise (TypeError "Used reassign operator on illegal types")

and bind_type id t (context, delta) =
  match t with
  | TArray (t, bf, s) ->
    Hashtbl.add context (id, None) (TArray (t, bf, s)); 
    add_array_banks s bf id bf (context, delta) t 0
  | other_exp -> 
    Hashtbl.add context (id, None) other_exp;
    (context, delta)

and check_funcdef id args body (context, delta) =
  (* FIXME: this is a little wonky *)
  List.iter (fun (e, t) -> ignore (bind_type e t (context, delta))) args;
  let (context', delta') = check_cmd body (context, delta) in
  (* List.iter (fun (e, t) -> Hashtbl.remove context' (e, None)) args; *)
  Hashtbl.add context' (id, None) (TFunc (List.map (fun (_, t) -> t) args));
  (context', delta')

and check_app id args (context, delta) =
  let argtypes = List.map (fun a -> check_expr a (context, delta) |> fst) args in
  match Hashtbl.find context (id, None) with
  | TFunc param_types -> 
    if List.exists2 
      (fun arg param -> not (types_equal delta arg param)) argtypes param_types
    then raise (TypeError ("Illegal arg type supplied to " ^ id))
    else (context, delta)
  | _ -> raise (TypeError (id ^ " is not a function and cannot be applied"))

and check_typedef id t (context, delta) =
  Hashtbl.add delta id t; (context, delta)


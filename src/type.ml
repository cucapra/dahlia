open Ast
open Context
open Error_msg

exception TypeError of string

let type_of_id id g =
  Context.get_binding id g

let type_of_alias_id id d =
  Context.get_alias_binding id d

let compute_bf lst =
  if List.length lst > 1 then failwith "TODO: md access"
  else List.fold_left (fun acc (_, b) -> acc * b) 0 lst

let rec types_equal delta t1 t2 =
  match t1, t2 with
  | TInt _, TInt _ -> true
  | TArray (a1, d1), TArray (a2, d2) -> d1=d2 && types_equal delta a1 a2
  | TIndex _, TIndex _ -> true
  | TAlias t1, t2 -> types_equal delta (Context.get_alias_binding t1 delta) t2
  | t1, TAlias t2 -> types_equal delta t1 (Context.get_alias_binding t2 delta)
  | t1, t2 -> t1=t2

let rec check_expr exp (context, (delta: Context.delta)) =
  match exp with
  | EInt (i, s)                  -> check_int i s (context, delta)
  | EFloat f                     -> check_float f (context, delta)
  | EBool _                      -> TBool, (context, delta)
  | EVar x                       -> Context.get_binding x context, (context, delta) 
  | EBinop (binop, e1, e2)       -> check_binop binop e1 e2 (context, delta)
  | EArray _                     -> raise (TypeError "Can't refer to array literal")
  | EPhysAccess (id, idx1, idx2) -> check_aa_expl id idx1 idx2 (context, delta)
  | EIndex idx                   -> failwith "Unreachable"
  | ELoglAccess (id, i)          -> check_aa_logl id i (context, delta)

and check_int i is_stat (ctx, dta) = (if is_stat then TInt (Some i) else TInt (None)), (ctx, dta)

and check_float f (ctx, dta) = TFloat, (ctx, dta)

and check_binop binop e1 e2 (c, d) =
  check_expr e1 (c, d)   |> fun (t1, (c1, d1)) ->
  check_expr e2 (c1, d1) |> fun (t2, (c2, d2)) ->
  try (Op_util.type_of_op t1 t2 d binop), (c2, d2)
  with Op_util.IllegalOperation -> 
    raise (TypeError (illegal_op binop t1 t2))

(* TODO: refactor this mess *)
and check_aa_expl id idx1 idx2 (c, d) =
  check_expr idx1 (c, d)   |> fun (idx1_t, (c1, d1)) ->
  check_expr idx2 (c1, d1) |> fun (idx2_t, (c2, d2)) ->
  match idx1_t, idx2_t, Context.get_binding id c2 with
  | TInt (Some i), TInt _, TArray (a_t, _)
  | TInt (Some i), TIndex _, TArray (a_t, _) -> 
    begin
      try a_t, (Context.consume_aa id i c2, d2) 
      with AlreadyConsumed i -> raise (TypeError (illegal_bank i id))
    end
  | TIndex (s, None), TInt _, TArray (a_t, _) 
  | TIndex (s, None), TIndex _, TArray (a_t, _) ->
    check_idx id s a_t (c, d)
  | TInt _, TInt _, TMux (m_id, s) 
  | TIndex _, TInt _, TMux (m_id, s) 
  | TInt _, TIndex _, TMux (m_id, s) 
  | TIndex _, TIndex _, TMux (m_id, s) ->
    begin
      match Context.get_binding id c2 with
      | TArray (a_t, banking) -> 
        let bf = compute_bf banking in 
        if s <= bf then a_t, (c, d)
        else raise (TypeError small_mux)
      | _ -> raise (TypeError illegal_mux)
    end
  | TMux (_, m_s), TIndex _, TArray (a_t, banking) ->
    let bf = compute_bf banking in
    if m_s <= bf then a_t, (c, d)
    else raise (TypeError small_mux)
  | TInt (None), _, _-> raise (TypeError static_bank_error)
  | t, _, _ -> 
    raise (TypeError (illegal_accessor_type t id))

and check_aa_logl id i (c, d) =
  match Context.get_binding id c, check_expr i (c, d) with
  | TMux (a_id, s), (TInt _, _) ->
    begin
      match Context.get_binding a_id c with
      | TArray (a_t, banking) -> 
        let bf = compute_bf banking in
        if s <= bf then a_t, (c, d)
        else raise (TypeError small_mux)
      | _ -> raise (TypeError illegal_mux)
    end
  | TArray (_, dims), (TIndex (s, d), _) ->
    let bf = compute_bf dims in
    if (bf mod (List.length s))=0 then failwith "implement me"
    else raise (TypeError (Error_msg.improper_unroll))

and check_idx id idx a_t (c, d) =
  let consume_indices = fun context bank ->
    try Context.consume_aa id bank context
    with AlreadyConsumed i -> raise (TypeError (illegal_bank bank id))
  in a_t, (Context.consume_aa_lst id idx c, d)

and check_aa_impl id i (c, d) =
  match check_expr i (c, d), Context.get_binding id c with
  | (TIndex (idxs, _), _), TArray (a_t, _) -> 
    check_idx id idxs a_t (c, d)
  | (TIndex _, _), t ->
    raise (TypeError (illegal_access id))
  | (t, _), _ -> 
    raise (TypeError (illegal_accessor_type t id))

let rec check_cmd cmd (context, (delta: Context.delta)) =
  match cmd with
  | CSeq (c1, c2)                  -> check_seq c1 c2 (context, delta)
  | CIf (cond, cmd)                -> check_if cond cmd (context, delta)
  | CFor (x, r1, r2, body)         -> check_for x r1 r2 body (context, delta)
  | CForImpl (x, r1, r2, u, body)  -> check_for_impl x r1 r2 body u (context, delta)
  | CAssign (x, e1)                -> check_assignment x e1 (context, delta)
  | CReassign (target, exp)        -> check_reassign target exp (context, delta)
  | CFuncDef (id, args, body)      -> check_funcdef id args body (context, delta)
  | CApp (id, args)                -> check_app id args (context, delta)
  | CTypeDef (id, t)               -> check_typedef id t (context, delta)
  | CMuxDef (mux_id, mem_id, size) -> check_muxdef mux_id mem_id size (context, delta)

and check_seq c1 c2 (context, delta) =
  check_cmd c1 (context, delta)
  |> fun (context', delta') -> check_cmd c2 (context', delta')

and check_if cond cmd (context, delta) =
  match check_expr cond (context, delta) with
  | TBool, (ctx', dta') -> check_cmd cmd (ctx', dta')
  | t, _ -> raise (TypeError (unexpected_type "conditional" t TBool))
  
and check_for id r1 r2 body (c, d) =
  check_expr r1 (c, d)   |> fun (r1_type, (c1, d1)) ->
  check_expr r2 (c1, d1) |> fun (r2_type, (c2, d2)) ->
  match r1_type, r2_type with
  | TInt _, TInt _ -> 
    check_cmd body (Context.add_binding id (TInt None) c2, d2)
  | _ -> raise (TypeError range_error)

and (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

and check_for_impl id r1 r2 body u (context, delta) =
  check_expr r1 (context, delta) |> fun (r1_type, (c1, d1)) ->
  check_expr r2 (context, delta) |> fun (r2_type, (c2, d2)) ->
  match r1_type, r2_type with
  | TInt (Some i1), TInt (Some i2) ->
    let range_size = i2 - i1 + 1 in
    if (range_size=u) then
      check_cmd body (Context.add_binding id (TIndex (0--(u-1), None)) c2, d2)
    else
      check_cmd body (Context.add_binding id (TIndex (0--(u-1), Some (range_size/u))) c2, d2)
  | _ -> raise (TypeError range_error)

and check_assignment id exp (context, delta) =
  check_expr exp (context, delta) |> fun (t, (c, d)) ->
  Context.add_binding id t c, delta

and check_reassign target exp (context, delta) =
  match target, exp with
  | EPhysAccess (id, idx1, idx2), expr ->
    check_aa_expl id idx1 idx2 (context, delta) |> fun (t_arr, (c, d)) ->
    check_expr exp (c, d)                       |> fun (t_exp, (c', d')) ->
    if types_equal delta t_arr t_exp then (c', d')
    else raise (TypeError (unexpected_type id t_exp t_arr))
  | EVar id, expr -> (context, delta)
  | ELoglAccess (id, idx), expr -> 
    check_aa_logl id idx (context, delta) |> fun (_, (c', d')) ->
    (c', d')
      (*
    check_aa_impl id idx (context, delta) |> fun (t_arr, (c, d)) ->
    check_expr expr (context, delta)      |> fun (t_exp, (c', d')) ->
    if types_equal delta t_arr t_exp then (c', d')
    else raise (TypeError "Tried to populate array with incorrect type") *)
  | _ -> raise (TypeError "Used reassign operator on illegal types")

and check_funcdef id args body (context, delta) =
  let context' = 
    List.fold_left 
      (fun ctx (arg_id, t) -> Context.add_binding arg_id t ctx) context args in
  let context'', delta'' = check_cmd body (context', delta) in
  Context.add_binding id (TFunc (List.map (fun (_, t) -> t) args)) context'', delta''
  (* List.iter (fun (e, t) -> Hashtbl.remove context' (e, None)) args; *)

and check_app id args (context, delta) =
  let argtypes = List.map (fun a -> check_expr a (context, delta) |> fst) args in
  match Context.get_binding id context with
  | TFunc param_types -> 
    List.iter2
      (fun arg param -> 
         if not (types_equal delta arg param)
         then raise (TypeError (unexpected_type 
          ("in function app of " ^ id ^ ", arg was") arg param))
         else ()) argtypes param_types;
    (context, delta)
  | _ -> raise (TypeError (illegal_app id))

and check_typedef id t (context, delta) =
  context, (Context.add_alias_binding id t delta)

and check_muxdef mux_id a_id size (context, delta) =
  Context.add_binding mux_id (TMux (a_id, size)) context, delta


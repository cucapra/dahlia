open Ast
open Context
open Error_msg

exception TypeError of string

let type_of_id id g =
  Context.get_binding id g

let type_of_alias_id id d =
  Context.get_alias_binding id d

let compute_bf lst =
  List.fold_left (fun acc (_, b) -> acc * b) 1 lst

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

let rec types_equal delta t1 t2 =
  match t1, t2 with
  | TArray (a1, d1), TArray (a2, d2) -> d1=d2 && types_equal delta a1 a2
  | TIndex _, TIndex _ -> true
  | TAlias t1, t2 -> types_equal delta (Context.get_alias_binding t1 delta) t2
  | t1, TAlias t2 -> types_equal delta t1 (Context.get_alias_binding t2 delta)
  | t1, t2 -> t1=t2

let rec check_expr exp (context, (delta: Context.delta)) =
  match exp with
  | EInt (i, s)                -> check_int i s (context, delta)
  | EFloat f                   -> check_float f (context, delta)
  | EBool _                    -> TBool, (context, delta)
  | EVar x                     -> Context.get_binding x context, (context, delta)
  | EBinop (binop, e1, e2)     -> check_binop binop e1 e2 (context, delta)
  | EBankedAA (id, idx1, idx2) -> check_banked_aa id idx1 idx2 (context, delta)
  | EAA (id, i)                -> check_aa id i (context, delta)

and check_int i is_stat (ctx, dta) = 
  (if is_stat then (TIndex ((i, i+1), (0, 1))) else (TIndex ((0, 1), (0, max_int)))), (ctx, dta)

and check_float f (ctx, dta) = TFloat, (ctx, dta)

and check_binop binop e1 e2 (c, d) =
  check_expr e1 (c, d)   |> fun (t1, (c1, d1)) ->
  check_expr e2 (c1, d1) |> fun (t2, (c2, d2)) ->
  try (Op_util.type_of_op t1 t2 d binop), (c2, d2)
  with Op_util.IllegalOperation -> 
    raise (TypeError (illegal_op binop t1 t2))

and check_banked_aa id idx1 idx2 (c, d) =
  check_expr idx1 (c, d)   |> fun (idx1_t, (c1, d1)) ->
  check_expr idx2 (c1, d1) |> fun (idx2_t, (c2, d2)) ->
  match idx1_t, idx2_t, Context.get_binding id c2 with
  | TIndex (s1, d1), TIndex (_, _), TArray (a_t, _) ->
    let (ls_1, hs_1) = s1 in
    let (ld_1, hd_1) = d1 in
    if ls_1 - hs_1 = 1 && hd_1 - ld_1 = max_int then
      begin
        try a_t, (Context.consume_aa id ls_1 c2, d2) 
        with AlreadyConsumed i -> raise (TypeError (illegal_bank i id))
      end
    else
      raise (TypeError static_bank_error)
  | t1, _, _ ->
    raise (TypeError (illegal_accessor_type t1 id))

and compute_unrollf idx_exprs (c, d) =
  match idx_exprs with
  | h::t ->
    begin
      match check_expr h (c, d) with
      | TIndex ((ls, hs), _), _ -> 
        if hs - ls = 1 then
          raise (TypeError "Index must contain static information")
        else
          (hs - ls) * compute_unrollf t (c, d)
      | _ -> raise (TypeError "Logical array access must be with idx types")
    end
  | [] -> 1

and check_aa id idx_exprs (c, d) =
  match Context.get_binding id c with
  | TArray (t, dims) ->
    let bf = compute_bf dims in
    let unrollf = compute_unrollf idx_exprs (c, d) in
    if (bf mod unrollf)=0 then
      try 
        t, (Context.consume_aa_lst id (0--(unrollf-1)) c, d)
      with AlreadyConsumed bank -> raise (TypeError (illegal_bank bank id)) 
    else 
      raise (TypeError "TypeError: unroll factor must be factor of banking factor")
  | _ -> raise (TypeError "TypeError: tried to index into non-array")

and check_idx id idx a_t (c, d) =
  a_t, (Context.consume_aa_lst id idx c, d)

let rec check_cmd cmd (context, delta) =
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
  | TIndex _, TIndex _ -> 
    check_cmd body (Context.add_binding id (TIndex ((0, 1), (0, max_int))) c2, d2)
  | _ -> raise (TypeError range_error)

and check_for_impl id r1 r2 body u (context, delta) =
  check_expr r1 (context, delta) |> fun (r1_type, (c1, d1)) ->
  check_expr r2 (context, delta) |> fun (r2_type, (c2, d2)) ->
  match r1_type, r2_type with
  | TIndex (st1, dyn1), TIndex (st2, dyn2) ->
    let (ls_1, hs_1) = st1 in
    let (ls_2, hs_2) = st2 in
    if (hs_1 - ls_1 = 1) && (hs_2 - ls_2 = 1) then (
      let range_size = ls_2 - ls_1 + 1 in
      if (range_size=u) then
        check_cmd body ((Context.add_binding id (TIndex ((0, u), (0, 1))) c2), d2)
      else
        check_cmd body ((Context.add_binding id (TIndex ((0, u), (0, (range_size/u)))) c2), d2))
    else raise (TypeError range_static_error)
  | _ -> raise (TypeError range_error)

and check_assignment id exp (context, delta) =
  check_expr exp (context, delta) |> fun (t, (c, d)) ->
  Context.add_binding id t c, delta

and check_reassign target exp (context, delta) =
  match target, exp with
  | EBankedAA (id, idx1, idx2), expr ->
    check_banked_aa id idx1 idx2 (context, delta) |> fun (t_arr, (c, d)) ->
    check_expr exp (c, d)                       |> fun (t_exp, (c', d')) ->
    if types_equal delta t_arr t_exp then (c', d')
    else raise (TypeError (unexpected_type id t_exp t_arr))
  | EVar id, expr -> (context, delta)
  | EAA (id, idx), expr -> 
    check_aa id idx (context, delta) |> fun (_, (c', d')) ->
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


open Ast
open Context
open Error_msg

exception TypeError of string

(** [compute_bf d] is a banking factor [b], computed from [d], where
 * [d] specifies the banking structure of some [TArray (t, d)].
 * This relies on the assumption that our ideas about banking across
 * multiple dimensions is correct; these ideas are detailed in
 * [https://capra.cs.cornell.edu/seashell/docs/indextype.html]. *)
let compute_bf lst =
  List.fold_left (fun acc (_, b) -> acc * b) 1 lst

(** Computes equality between [t1] and [t2] and handles type aliases.
 *  Also special cases [TIndex] so that any two [TIndex] are equal. *)
let rec types_equal (delta : delta) (t1 : type_node) (t2 : type_node) : bool =
  match t1, t2 with
  | TArray (a1, d1), TArray (a2, d2) -> d1=d2 && types_equal delta a1 a2
  | TIndex _, TIndex _ -> true
  | TAlias ta, t | t, TAlias ta ->
      types_equal delta t (Context.get_alias_binding ta delta)
  | t1, t2 -> t1=t2

(** [check_expr exp (ctx, delta)] is [t, (ctx', delta')], where [t] is the
 * type of [exp] under context (ctx, delta) and (ctx', delta') is an updated
 * context resulting from evaluating [exp]. Raises [TypeError s] if there
 * is a type error in [exp].*)
let rec check_expr (exp : expression) (ctx, (delta: Context.delta)) =
  match exp with
  | EFloat _                   -> TFloat, (ctx, delta)
  | EBool _                    -> TBool, (ctx, delta)
  | EInt (i, s)                -> check_int i s, (ctx, delta)
  | EVar x                     -> Context.get_binding x ctx, (ctx, delta)
  | EBinop (binop, e1, e2)     -> check_binop binop e1 e2 (ctx, delta)
  | EBankedAA (id, idx1, idx2) -> check_banked_aa id idx1 idx2 (ctx, delta)
  | EAA (id, i)                -> check_aa id i (ctx, delta)

(** [check_int i is_stat] is [TIndex (s, d)], the index type representation
 * of the integer [i], which could be static or non-static, depending on
 * [is_stat]. This representation is detailed in
 * [https://capra.cs.cornell.edu/seashell/docs/indextype.html]. *)
and check_int i is_stat =
  if is_stat then
    TIndex ((i, i+1), (0, 1))
  else
    TIndex ((0, 1), (min_int, max_int))

and check_binop binop e1 e2 (c, d) =
  let (t1, (c1, d1)) = check_expr e1 (c, d) in
  let (t2, (c2, d2)) = check_expr e2 (c1, d1) in
  try (Op_util.type_of_op t1 t2 d binop), (c2, d2)
  with Op_util.IllegalOperation ->
    raise (TypeError (illegal_op binop t1 t2))

(* [check_banked_aa id idx1 idx2 (c, d)] represents a _banked
 * array access_, with a bank number specified by [idx1] and an index into this
 * bank specified by [idx2]. It is the value [t, (c', d')] where [t] is the
 * type of the elements of array [id], and (c', d') is an updated context
 * resulting from evaluating [idx1] and [idx2], and consuming the appopriate
 * indices of array [id]. Raises [TypeError s] if:
 *   - [idx1] or [idx2] are illegal types (non-index types)
 *   - illegal banks are accessed (i.e. already-consumed indices) *)
and check_banked_aa id idx1 idx2 (c, d) =
  let (idx1_t, (c1, d1)) = check_expr idx1 (c, d) in
  let (idx2_t, (c2, d2)) = check_expr idx2 (c1, d1) in
  match idx1_t, idx2_t, Context.get_binding id c2 with
  | TIndex (s1, d1), TIndex (_, _), TArray (a_t, _) ->
    begin
      let (ls_1, hs_1) = s1 in
      let (ld_1, hd_1) = d1 in
      if hs_1 - ls_1 = 1 && hd_1 - ld_1 = 1 then
        begin
          try a_t, (Context.consume_aa id ls_1 c2, d2)
          with AlreadyConsumed i -> raise (TypeError (illegal_bank i id))
        end
      else
        raise (TypeError static_bank_error)
    end
  | t1, _, _ ->
    raise (TypeError (illegal_accessor_type t1 id))

(** [compute_unrollf idx_exprs (c, d)] is the unroll factor [u] implied by
 * the index type accessors [idx_exprs], each of which should be of type
 * [TIndex (s, d)]. Raises [TypeError s] if [idx_exprs] contains invalid
 * array access types, i.e. non-index types. *)
and compute_unrollf idx_exprs (c, d) =
  match idx_exprs with
  | h::t ->
    begin
      match check_expr h (c, d) with
      | TIndex ((ls, hs), _), _ -> (hs - ls) * compute_unrollf t (c, d)
      | _ -> raise (TypeError "Logical array access must be with idx types")
    end
  | [] -> 1

and check_aa id idx_exprs (c, d) =
  match Context.get_binding id c with
  | TArray (t, dims) ->
    let num_dimensions = List.length dims in
    let access_dimensions = List.length idx_exprs in
    if num_dimensions != access_dimensions
      then raise
        (TypeError (incorrect_aa_dims id num_dimensions access_dimensions))
    else
      let bf = compute_bf dims in
      let unrollf = compute_unrollf idx_exprs (c, d) in
      if (bf mod unrollf)=0 then
        try
          let banks = Core.List.range 0 (unrollf-1) in
          t, (Context.consume_aa_lst id banks c, d)
        with AlreadyConsumed bank -> raise @@ TypeError (illegal_bank bank id)
      else
        raise (TypeError "TypeError: unroll factor must be factor of banking factor")
  | _ -> raise (TypeError "TypeError: tried to index into non-array")

let rec check_cmd cmd (ctx, delta) =
  match cmd with
  | CSeq (c1, c2)                  -> check_seq c1 c2 (ctx, delta)
  | CIf (cond, cmd)                -> check_if cond cmd (ctx, delta)
  | CFor (x, r1, r2, uo, body)     -> check_for_impl x r1 r2 body uo (ctx, delta)
  | CAssign (x, e1)                -> check_assignment x e1 (ctx, delta)
  | CReassign (target, exp)        -> check_reassign target exp (ctx, delta)
  | CFuncDef (id, args, body)      -> check_funcdef id args body (ctx, delta)
  | CApp (id, args)                -> check_app id args (ctx, delta)
  | CTypeDef (id, t)               -> check_typedef id t (ctx, delta)
  | CMuxDef (mux_id, mem_id, size) -> check_muxdef mux_id mem_id size (ctx, delta)

and check_seq c1 c2 (ctx, delta) =
  check_cmd c1 (ctx, delta) |> fun (ctx', delta') -> check_cmd c2 (ctx', delta')

and check_if cond cmd (ctx, delta) =
  match check_expr cond (ctx, delta) with
  | TBool, (ctx', dta') -> check_cmd cmd (ctx', dta')
  | t, _ -> raise (TypeError (unexpected_type "conditional" t TBool))

and check_for_impl id r1 r2 body u (ctx, delta) =
  let r1_type, (ctx1, d1) = check_expr r1 (ctx, delta) in
  let r2_type, (ctx2, d2) = check_expr r2 (ctx1, d1) in
  match r1_type, r2_type with
  | TIndex (st1, _), TIndex (st2, _) ->
    let (ls_1, hs_1) = st1 in
    let (ls_2, hs_2) = st2 in
    if (hs_1 - ls_1 = 1) && (hs_2 - ls_2 = 1) then
      let range_size = ls_2 - ls_1 + 1 in
      let typ = TIndex ((0, u), (0, (range_size/u)))
      in check_cmd body (Context.add_binding id typ ctx2, d2)
    else raise (TypeError range_static_error)
  | _ -> raise (TypeError range_error)

(** [check_assignment id exp (c, d)] is [(c', d')], where [(c', d')]
 * contain a binding from [id] to the type of expression [exp] under
 * the context (c, d). *)
and check_assignment id exp (ctx, delta) =
  let (t, (c, d)) = check_expr exp (ctx, delta) in
  (Context.add_binding id t c), d

(* TODO(ted): rethink this *)
and check_reassign target exp (ctx, delta) =
  match target, exp with
  | EBankedAA (id, idx1, idx2), _ ->
    check_banked_aa id idx1 idx2 (ctx, delta) |> fun (t_arr, (c, d)) ->
    check_expr exp (c, d)                     |> fun (t_exp, (c', d')) ->
    if types_equal delta t_arr t_exp then (c', d')
    else raise @@ TypeError (unexpected_type id t_exp t_arr)
  | EVar _, _ -> (ctx, delta)
  | EAA (id, idx), _ ->
    snd @@ check_aa id idx (ctx, delta)
  | _ -> raise (TypeError "Used reassign operator on illegal types")

(** [check_funcdef id args body (ctx, delta)] is [(ctx', delta')], where
 * [ctx'] contains a new binding [id] to [TFunc args], and [delta'] contains
 * possibly new type alias bindings.
 *   - [args] is a list [(a1, t1)..(an, tn)], where [ai] is an argument of
       the function with type [ti]. The order of the list implies the order
       of the function arguments.
     - [body] is a command representing the body of the function. *)
and check_funcdef id args body (ctx, delta) =
  let add_argbind = fun ctx (arg_id, t) -> Context.add_binding arg_id t ctx in
  let context' = List.fold_left add_argbind ctx args in
  let context'', delta' = check_cmd body (context', delta) in
  let argtypes = List.map (fun (_, t) -> t) args in
  Context.add_binding id (TFunc argtypes) context'', delta'

and check_app id args (ctx, delta) =
  let check_params arg param =
    if not (types_equal delta arg param)
    then raise @@ TypeError (
      unexpected_type ("in function app of " ^ id ^ ", arg was") arg param)
    else () in
  let argtypes = List.map (fun a -> fst @@ check_expr a (ctx, delta)) args in
  match Context.get_binding id ctx with
  | TFunc param_types -> List.iter2 check_params argtypes param_types; (ctx, delta)
  | _ -> raise (TypeError (illegal_app id))

and check_typedef id t (ctx, delta) = ctx, (Context.add_alias_binding id t delta)

(** [check_muxdef mux_id a_id size (c, d)] produces a context [(c', d')] containing
 * a new binding from [id] to [TMux (a_id, size)], where:
 *   - [m_id] is the id of a multiplexer
 *   - [a_id] is the id of the memory encapsulated by mux [m_id]
     - [size] is the number of inputs for the mux [m_id] *)
and check_muxdef mux_id a_id size (ctx, delta) =
  Context.add_binding mux_id (TMux (a_id, size)) ctx, delta

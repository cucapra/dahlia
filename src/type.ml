open Ast
open Context
open Error_msg

let types_eq t1 t2 = match t1, t2 with
  | TIndex _, TIndex _ -> true
  | _ -> t1 = t2

let is_static = function
  | TIndex ((ls, hs), (ld, hd)) -> (hs - ls = 1) && (hd - ld = 1)
  | _ -> false

(** [check_expr exp (ctx, delta)] is [t, (ctx', delta')], where [t] is the
 * type of [exp] under context (ctx, delta) and (ctx', delta') is an updated
 * context resulting from type-checking [exp]. Raises [TypeError s] if there
 * is a type error in [exp].*)
let rec check_expr exp ctx : type_node * gamma =
  match exp with
  | EFloat _                   -> TFloat, ctx
  | EBool _                    -> TBool, ctx
  | EInt i                     -> TIndex ((i, i+1), (0, 1)), ctx
  | EVar x                     -> Context.get_binding x ctx, ctx
  | EBinop (binop, e1, e2)     -> check_binop binop e1 e2 ctx
  | EAA _        ->
      raise (TypeError "Array access expression cannot occur outside capability commands")

(** [check_binop b e1 e2 (c, d)] is [(t, (c', d')], where [t] is the type of
 * the expression [EBinop (b, e1, e2)] and [(c', d')] is the updated context
 * resulting from the type-checking of [e1] and [e2]. *)
and check_binop binop e1 e2 c : type_node * gamma =
  let (t1, c1) = check_expr e1 c in
  let (t2, c2) = check_expr e2 c1 in
  try (Op_util.type_of_op t1 t2 binop), c2
  with Op_util.IllegalOperation -> raise @@ TypeError (illegal_op binop t1 t2)

(* [check_banked_aa id idx1 idx2 (c, d)] represents a _banked
 * array access_, with a bank number specified by [idx1] and an index into this
 * bank specified by [idx2]. It is the value [t, (c', d')] where [t] is the
 * type of the elements of array [id], and (c', d') is an updated context
 * resulting from type-checking [idx1] and [idx2], and consuming the appopriate
 * indices of array [id]. Raises [TypeError s] if:
 *   - [idx1] or [idx2] are illegal types (non-index types)
 *   - illegal banks are accessed (i.e. already-consumed indices)
and check_banked_aa id idx1 idx2 c : type_node * gamma =
  let idx1_t, c1 = check_expr idx1 c in
  let idx2_t, c2 = check_expr idx2 c1 in
  match idx1_t, idx2_t, Context.get_binding id c2 with
  | TIndex ((ls_1, _), _) as t1, (TIndex (t2s, _) as t2), TArray (a_t, _) ->
    if not (is_svalue t2s) then raise @@ TypeError (illegal_banked_aa t2 ls_1 id)
    else
      if is_static t1 then
        try a_t, Context.consume_binding id ls_1 c2
        with AlreadyConsumed i -> raise @@ TypeError (illegal_bank i id)
      else
        raise (TypeError static_bank_error)
  | t1, _, _ -> raise @@ TypeError (illegal_accessor_type t1 id)

*)

(** FIXME(rachit): Gamma needs to be correctly thread through in List.map *)
and get_unroll_factors id idx_exprs c =
  let get_unroll_factor idx = match check_expr idx c with
    | TIndex ((ls, hs), _), _ -> hs - ls
    | t, _ ->  raise @@ TypeError (illegal_accessor_type t id)
  in (List.map get_unroll_factor idx_exprs, c)

(** [check_aa id idx_exprs (c, d)] represents a _normal array access_,
 * accessing array with identifier [id], where [idx_exprs] is a list of
 * expressions representing the index accessors. It produces [t, (c', d')],
 * where [t] is the type of the array elements of array [id], and [(c', d')] is
 * the updated context resulting from typechecking each expression in [idx_exprs]
 * and consuming the appropriate array indices. Raises [TypeError s] if:
 *  - an expression in [idx_exprs] is an illegal type (i.e. not TIndex _)
    - illegal banks are accessed (i.e. already consumed banks) *)
and check_aa id idx_exprs c : type_node * gamma =
  match Context.get_binding id c with
  | TArray (t, dims) ->
    let num_dimensions = List.length dims in
    let access_dimensions = List.length idx_exprs in
    if num_dimensions != access_dimensions
      then raise
        (TypeError (incorrect_aa_dims id num_dimensions access_dimensions))
    else
      let bfs = List.map (fun (_, f) -> f) dims
      and (ufs, c) = get_unroll_factors id idx_exprs c in
      if (List.for_all2 (fun bf uf -> bf = uf) bfs ufs) then
        try
          t, (Context.consume_binding id c)
        with AlreadyConsumed -> raise @@ TypeError (illegal_bank id)
      else
        raise (TypeError improper_unroll)
  | _ -> raise @@ TypeError (not_an_array id)


(** [check_cmd cmd (c, d)] is [(c', d')], an updated context resutling from
 * type-checking command [cmd], Raises [TypeError s]. *)
let rec check_cmd cmd ctx : gamma =
  match cmd with
  | CSeq clist     -> check_seq clist ctx
  | CIf args       -> check_if args ctx
  | CFor args      -> check_for args ctx
  | CAssign args   -> check_assignment args ctx
  | CReassign args -> check_reassign args ctx
  | CFuncDef args  -> check_funcdef args ctx
  | CApp args      -> check_app args ctx
  | CCap args      -> check_cap args ctx
  | CExpr expr     -> snd @@ check_expr expr ctx
  | CEmpty         -> ctx
  | CTypeDef _     -> raise (Failure "Impossible: Found CTypeDef in AST")

and check_seq clist ctx =
  let f c cmd = check_cmd cmd c in
  List.fold_left f ctx clist

(** [check_if cond cmd (c, d)] is [(c', d')], an updated context resulting from
 * type-checking [cond], followed by [cmd]. *)
and check_if (cond, cmd) ctx =
  match check_expr cond ctx with
  | TBool, ctx' -> check_cmd cmd ctx'
  | t, _ -> raise (TypeError (unexpected_type "conditional" t TBool))

(** [check_for id r1 r2 body u (c, d)] is [(c', d')], an updated context resulting
 * from evaluating the following:
 *   - [r1] and [r2], which are the start and ends of the for loop counter variable;
 *     these should be static ints (represented by index types)
 *   - [body], the body of the for loop
 * Additionally, unroll factor [u] influences the type bound to [id] (see Seashell
 * notes) *)
and check_for (id, r1, r2, u, body, _) ctx =
  let r1_type, ctx1 = check_expr r1 ctx in
  let r2_type, ctx2 = check_expr r2 ctx1 in
  match r1_type, r2_type with
  | TIndex ((ls_1, _), _) as t1, (TIndex ((ls_2, _), _) as t2) ->
    if is_static t1 && is_static t2 then
      let range_size = ls_2 - ls_1 + 1 in
      let typ = TIndex ((0, u), (0, (range_size/u)))
      in check_cmd body (Context.add_binding id typ ctx2)
    else raise @@ TypeError (range_static_error t1 t2)
  | t1, t2 -> raise @@ TypeError (range_error t1 t2)

(** [check_assignment id exp (c, d)] is [(c', d')], where [(c', d')]
 * contain a binding from [id] to the type of expression [exp] under
 * the context (c, d). *)
and check_assignment (id, exp) ctx =
  let (t, c) = check_expr exp ctx in
  Context.add_binding id t c

(** [check_bitsizes target v] is (). It raises [TypeError] if [target]
 * is represented with less bits than [v]. *)
and check_bitsizes t1 t2 =
  match t1, t2 with
  | TIndex (_, (ld1, hd1)), TIndex (_, (ld2, hd2)) ->
    if hd1-ld1 < hd2-ld2 then
      raise @@ TypeError (reassign_bit_violation t1 t2)
  | _ -> ()

(** [check_reassign target exp (c, d)] is an updated context [(c', d')],
 * resulting from type-checking [target] and [exp]. [target] could be:
 *   - an array access, banked or not
 *   - a variable that already is bound *)
and check_reassign (target, expr) ctx =
  match target with
  | EVar id ->
      let (typ, c1) = check_expr expr ctx in
      (match get_binding id ctx with
      | TLin t when (types_eq t typ) ->
          check_bitsizes t typ;
          begin try Context.consume_binding id ctx
          with AlreadyConsumed -> raise @@ TypeError (illegal_bank id) end
      | t when (types_eq t typ) ->
        check_bitsizes t typ; c1
      | _ -> raise @@ TypeError (reassign_type_mismatch (get_binding id ctx) typ))
  | EAA (id, _) -> raise @@ TypeError (invalid_array_write id)
  | _ -> raise (TypeError "Used reassign operator on illegal types")

(** [check_funcdef id args body (ctx, delta)] is [(ctx', delta')], where
 * [ctx'] contains a new binding [id] to [TFunc args], and [delta'] contains
 * possibly new type alias bindings.
 *   - [args] is a list [(a1, t1)..(an, tn)], where [ai] is an argument of
       the function with type [ti]. The order of the list implies the order
       of the function arguments.
     - [body] is a command representing the body of the function. *)
and check_funcdef (id, args, body) ctx =
  let add_argbind = fun ctx (arg_id, t) -> Context.add_binding arg_id t ctx in
  let context' = List.fold_left add_argbind ctx args in
  let context'' = check_cmd body context' in
  let argtypes = List.map (fun (_, t) -> t) args in
  Context.add_binding id (TFunc argtypes) context''

(** [check_app id args (c, d)] is [(c', d')], where [(c', d')] is the
 * context resulting from type-checking the function with identifier [id] with
 * the arguments represented by [args], where [args] an expression list. *)
and check_app (id, args) ctx =
  let check_params arg param =
    if not (arg = param) then
      raise @@ TypeError (
        unexpected_type ("in function app of " ^ id ^ ", arg was") arg param)
    else () in
  let argtypes = List.map (fun a -> fst @@ check_expr a ctx) args in
  match Context.get_binding id ctx with
  | TFunc param_types -> List.iter2 check_params argtypes param_types; ctx
  | _ -> raise @@ TypeError (illegal_app id)

and check_cap (cap, expr, id) ctx : gamma =
  let (t, c) = match expr with
    | EAA (arr, idxs) -> check_aa arr idxs ctx
    | _ -> raise (TypeError cap_non_array)
  in
  Context.add_binding id (if cap = Read then t else TLin t) c

let typecheck cmd : gamma = check_cmd cmd empty_gamma

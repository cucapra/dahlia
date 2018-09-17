open Ast
open Context
open Error_msg

exception TypeError of string

(** [compute_bf dims] is a banking factor [b], computed from [dims], where
 * [d] specifies the banking structure of some [TArray (t, dims)].
 * This relies on the assumption that our ideas about banking across
 * multiple dimensions is correct; these ideas are detailed in
 * [https://capra.cs.cornell.edu/seashell/docs/indextype.html]. *)
let compute_bf dims =
  List.fold_left (fun acc (_, b) -> acc * b) 1 dims

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
  | EBankedAA (id, idx1, idx2) -> check_banked_aa id idx1 idx2 ctx
  | EAA (id, i)                -> check_aa id i ctx

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
 *   - illegal banks are accessed (i.e. already-consumed indices) *)
and check_banked_aa id idx1 idx2 c : type_node * gamma =
  let idx1_t, c1 = check_expr idx1 c in
  let idx2_t, c2 = check_expr idx2 c1 in
  match idx1_t, idx2_t, Context.get_binding id c2 with
  | TIndex (s1, d1), TIndex (_, _), TArray (a_t, _) ->
    let (ls_1, hs_1) = s1 and (ld_1, hd_1) = d1 in
    if hs_1 - ls_1 = 1 && hd_1 - ld_1 = 1 then
      begin
        try a_t, Context.consume_aa id ls_1 c2
        with AlreadyConsumed i -> raise @@ TypeError (illegal_bank i id)
      end
    else
      raise (TypeError static_bank_error)
  | t1, _, _ -> raise @@ TypeError (illegal_accessor_type t1 id)

(** [compute_unrollf idx_exprs (c, d)] is the unroll factor [u] implied by
 * the index type accessor expressions [idx_exprs], each of which should be of
 * type [TIndex (s, d)]. Raises [TypeError s] if [idx_exprs] contains invalid
 * array access types, i.e. non-index types. *)
and compute_unrollf idx_exprs c =
  match idx_exprs with
  | h::t ->
    begin
      match check_expr h c with
      | TIndex ((ls, hs), _), _ -> (hs - ls) * compute_unrollf t c
      | _ -> raise (TypeError "Logical array access must be with idx types")
    end
  | [] -> 1

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
      let bf = compute_bf dims in
      let unrollf = compute_unrollf idx_exprs c in
      if (bf mod unrollf)=0 then
        try
          let banks = Core.List.range 0 (unrollf-1) in
          t, (Context.consume_aa_lst id banks c)
        with AlreadyConsumed bank -> raise @@ TypeError (illegal_bank bank id)
      else
        raise (TypeError "TypeError: unroll factor must be factor of banking factor")
  | _ -> raise (TypeError "TypeError: tried to index into non-array")

(** [check_cmd cmd (c, d)] is [(c', d')], an updated context resutling from
 * type-checking command [cmd], Raises [TypeError s]. *)
let rec check_cmd cmd ctx : gamma =
  match cmd with
  | CSeq clist                     -> check_seq clist ctx
  | CIf (cond, cmd)                -> check_if cond cmd ctx
  | CFor (x, r1, r2, uo, body)     -> check_for x r1 r2 body uo ctx
  | CAssign (x, e1)                -> check_assignment x e1 ctx
  | CReassign (target, exp)        -> check_reassign target exp ctx
  | CFuncDef (id, args, body)      -> check_funcdef id args body ctx
  | CApp (id, args)                -> check_app id args ctx
  | CTypeDef _                     -> ctx
  | CMuxDef (mux_id, mem_id, size) -> check_muxdef mux_id mem_id size ctx
  | CExpr expr                     -> snd @@ check_expr expr ctx
  | CWrite _                       -> failwith "capabilities not implemented"

and check_seq clist ctx =
  let f c cmd = check_cmd cmd c in
  List.fold_left f ctx clist

(** [check_if cond cmd (c, d)] is [(c', d')], an updated context resulting from
 * type-checking [cond], followed by [cmd]. *)
and check_if cond cmd ctx =
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
and check_for id r1 r2 body u ctx =
  let r1_type, ctx1 = check_expr r1 ctx in
  let r2_type, ctx2 = check_expr r2 ctx1 in
  match r1_type, r2_type with
  | TIndex (st1, _), TIndex (st2, _) ->
    let (ls_1, hs_1) = st1 in
    let (ls_2, hs_2) = st2 in
    if (hs_1 - ls_1 = 1) && (hs_2 - ls_2 = 1) then
      let range_size = ls_2 - ls_1 + 1 in
      let typ = TIndex ((0, u), (0, (range_size/u)))
      in check_cmd body (Context.add_binding id typ ctx2)
    else raise (TypeError range_static_error)
  | _ -> raise (TypeError range_error)

(** [check_assignment id exp (c, d)] is [(c', d')], where [(c', d')]
 * contain a binding from [id] to the type of expression [exp] under
 * the context (c, d). *)
and check_assignment id exp ctx =
  let (t, c) = check_expr exp ctx in
  Context.add_binding id t c

(** [check_reassign target exp (c, d)] is an updated context [(c', d')],
 * resulting from type-checking [target] and [exp]. [target] could be:
 *   - an array access, banked or not
 *   - a variable that already is bound *)
and check_reassign target exp ctx =
  match target, exp with
  | EBankedAA (id, idx1, idx2), _ ->
    check_banked_aa id idx1 idx2 ctx |> fun (t_arr, c) ->
    check_expr exp c                 |> fun (t_exp, c') ->
    if t_arr = t_exp then c'
    else raise @@ TypeError (reassign_type_mismatch t_exp t_arr)
  | EVar id, expr ->
      let (typ, c1) = check_expr expr ctx in
      if get_binding id ctx = typ then c1
      else raise @@ TypeError (reassign_type_mismatch (get_binding id ctx) typ)
  | EAA (id, idx), expr ->
      let (tl, c1) = check_aa id idx ctx in
      let (tr, c2) = check_expr expr c1 in
      if tl = tr then c2
      else raise @@ TypeError (reassign_type_mismatch tl tr)
  | _ -> raise (TypeError "Used reassign operator on illegal types")

(** [check_funcdef id args body (ctx, delta)] is [(ctx', delta')], where
 * [ctx'] contains a new binding [id] to [TFunc args], and [delta'] contains
 * possibly new type alias bindings.
 *   - [args] is a list [(a1, t1)..(an, tn)], where [ai] is an argument of
       the function with type [ti]. The order of the list implies the order
       of the function arguments.
     - [body] is a command representing the body of the function. *)
and check_funcdef id args body ctx =
  let add_argbind = fun ctx (arg_id, t) -> Context.add_binding arg_id t ctx in
  let context' = List.fold_left add_argbind ctx args in
  let context'' = check_cmd body context' in
  let argtypes = List.map (fun (_, t) -> t) args in
  Context.add_binding id (TFunc argtypes) context''

(** [check_app id args (c, d)] is [(c', d')], where [(c', d')] is the
 * context resulting from type-checking the function with identifier [id] with
 * the arguments represented by [args], where [args] an expression list. *)
and check_app id args ctx =
  let check_params arg param =
    if not (arg = param)
    then raise @@ TypeError (
      unexpected_type ("in function app of " ^ id ^ ", arg was") arg param)
    else () in
  let argtypes = List.map (fun a -> fst @@ check_expr a ctx) args in
  match Context.get_binding id ctx with
  | TFunc param_types -> List.iter2 check_params argtypes param_types; ctx
  | _ -> raise @@ TypeError (illegal_app id)

(** [check_muxdef mux_id a_id size (c, d)] produces a context [(c', d')] containing
 * a new binding from [id] to [TMux (a_id, size)], where:
 *   - [m_id] is the id of a multiplexer
 *   - [a_id] is the id of the memory encapsulated by mux [m_id]
     - [size] is the number of inputs for the mux [m_id] *)
and check_muxdef mux_id a_id size ctx =
  Context.add_binding mux_id (TMux (a_id, size)) ctx

let typecheck cmd : gamma = check_cmd cmd empty_gamma

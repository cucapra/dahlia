open Ast
open Ast_visitor

module IdMap = Map.Make(struct
    type t = id
    let compare = String.compare
end)

(* Maps [id]s of views to [e], where [e] is the id's view
 * expression, containing [(id' off, w, s)]. ([id'] is what [id] is
 * constructed from.) *)
type view_map = expr IdMap.t

(* [resolve_aa_id id vmap] is an id [id'], where [id'] is either:
 *   - [id], if [id] is not a view
 *   - the identifier of the array that the view [id] is originally
 *     constructed from
 * This function makes the assumption that all array accesses are valid,
 * i.e. there is no access a[i] where a is unbound. This should be
 * guaranteed, because such errors would be caught in the typechecker
 * pass. *)
let rec resolve_aa_id id vmap =
  try
    let v_id =
      match IdMap.find id vmap with
      | EView (id, _) -> id
      | _ ->
        failwith "Impossible; view resolving visitor found non-view in viewmap"
    in resolve_aa_id v_id vmap
  with Not_found -> id

let rec resolve_aa_exprs id idxs vmap ctx =
  try
    match IdMap.find id vmap with
    | EView (id, params) ->
      let idxs' = resolve_aa_exprs id idxs vmap ctx in
      let f a idx (off, _, s) =
        a @ [EBinop (BopPlus, off, (EBinop (BopTimes, idx, EInt s)))]
      in List.fold_left2 f [] idxs' params
    | _ ->
      failwith "Impossible; view resolving visitor found non-view in viewmap"
  with Not_found -> idxs

(* Views should be resolved after typechecking and before compiling. *)
class view_resolver = object(self)
  inherit [Context.gamma * view_map] ast_mapper

  (* If we made a view, map its id to what we made the view on, and
   * delete this assignment command; the compiler doesn't want it. *)
  method! private cassign (id, e) (ctx, view_map) =
    match e with
    | EView _ ->
      let view_map' = IdMap.add id e view_map in
      CEmpty, (ctx, view_map')
    | _               ->
      let e', st' = self#expr e (ctx, view_map) in
      CAssign (id, e'), st'

  (* Transform array accesses on views into array accesses on
   * the original array. *)
  method! private eaa (id, is) (ctx, view_map) =
    match Context.get_binding id ctx with
    | TArray _ ->
      let id' = resolve_aa_id id view_map in
      let is' = resolve_aa_exprs id is view_map ctx in
      EAA (id', is'), (ctx, view_map)
    | _ ->
      failwith "View resolving visitor detected array access on non-array."
end

let remove_views cmd ctx =
  (new view_resolver)#command cmd (ctx, IdMap.empty) |> fst

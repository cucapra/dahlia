open Ast
open Ast_visitor

(* [resolve_aa_id v] is the [a_id], the id of the array from which
 * [v] is constructed, directly or indirectly. [v] is directly
 * constructed from [a_id] if it's defined as a view on [a_id]. [v]
 * is indirectly constructed from [a_id] if it's defined on a view
 * that's directly or indirectly constructed from [a_id]. *)
let rec resolve_aa_id v ctx =
  match v with
  | TView (_, Base, (id, _, _, _)) -> id
  | TView (_, View, (id, _, _, _)) ->
    let v' = Context.get_binding id ctx in
    resolve_aa_id v' ctx
  | _ ->
    failwith "Impossible case; [resolve_aa_id v ctx] only called on TViews."

let rec resolve_aa_expr e v ctx =
  match v with
  | TView (_, Base, (_, off, _, s)) ->
    EBinop (BopPlus, EInt (off), (EBinop (BopTimes, e, EInt s)))
  | TView (_, View, (id, off, _, s)) ->
    let v' = Context.get_binding id ctx in
    let descend = resolve_aa_expr e v' ctx in
    EBinop (BopPlus, EInt (off), (EBinop (BopTimes, descend, EInt s)))
  | _ ->
    failwith "Impossible case; [resolve_aa_expr v ctx] only called on TViews."

(* Views should be resolved after typechecking and before compiling. *)
class view_resolver = object(self)
  inherit [Context.gamma] ast_mapper

  (* Remove declarations of views; the compiler doesn't want them. *)
  method! private cassign (id, e) ctx =
    match Context.get_binding id ctx with
    | TView _ -> CEmpty, ctx
    | _ -> let e', st' = self#expr e ctx in
      CAssign (id, e'), st'

  (* Transform array accesses on views into array accesses on
   * the original array. *)
  method! private eaa (id, is) ctx =
    let e = match is with
    | [i1] -> i1
    | _ -> failwith "NYI md views" in
    match Context.get_binding id ctx with
    | TArray _ -> EAA (id, [e]), ctx
    | TView _ as t ->
      let id' = resolve_aa_id t ctx in
      let e'  = resolve_aa_expr e t ctx in
      EAA (id', [e']), ctx
    | _ ->
      failwith "View resolving visitor detected array access on non-array."

end

let remove_views cmd ctx =
  (new view_resolver)#command cmd ctx |> fst

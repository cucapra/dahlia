open Ast
open Ast_visitor

module StringMap =
  Map.Make(struct type t = id;; let compare = String.compare end)

type delta = type_node StringMap.t

let add_alias_binding (id, t) d =
  StringMap.add id t d

let get_alias_binding id d =
  try StringMap.find id d
  with Not_found -> raise (Context.NoBinding id)

<<<<<<< HEAD
class type_alias_resolver = object
  inherit [delta] ast_mapper

  (** Resolve binding using context *)
  method! private talias id st = get_alias_binding id st, st

  (** Add binding for type *)
  method! private ctypedef bind st =
    CTypeDef (fst @@ bind, snd @@ bind), add_alias_binding bind st

  (** Don't visit expressions *)
  method! private expr e st = e, st

end

let remove_aliases cmd =
  (new type_alias_resolver)#command cmd StringMap.empty |> fst
=======
let rec resolve_type typ dta = match typ with
  | TBool | TFloat | TMux _ | TIndex _ -> typ
  | TAlias id -> get_alias_binding id dta
  | TLin t -> TLin (resolve_type t dta)
  | TArray (t, ts) -> TArray (resolve_type t dta, ts)
  | TFunc tlist -> TFunc (List.map (fun t -> resolve_type t dta) tlist)

let rec resolve_cmd_seq clst dta = match clst with
  | [] -> ([], dta)
  | cmd :: tl ->
      let (c1, dta1) = resolve_cmd cmd dta in
      let (ctl, dta2) = resolve_cmd_seq tl dta1 in
      c1 :: ctl, dta2

and resolve_cmd cmd dta : command * delta = match cmd with
  | CMuxDef _ | CWrite _ | CAssign _ | CReassign _ | CApp _ | CExpr _ -> cmd, dta
  | CIf (e, c) ->
      let (c1, dta1) = resolve_cmd c dta in
      CIf (e, c1), dta1
  | CFor (id, e1, e2, un, c) ->
      let (c1, dta1) = resolve_cmd c dta in
      CFor (id, e1, e2, un, c1), dta1
  | CSeq clst ->
      let (clst1, dta1) = resolve_cmd_seq clst dta in
      CSeq clst1, dta1
  | CFuncDef (id, id_typ_lst, c) ->
      let id_typ_lst1 = List.map (fun (i, t) -> i, resolve_type t dta) id_typ_lst in
      let (c1, dta1) = resolve_cmd c dta in
      CFuncDef(id, id_typ_lst1, c1), dta1
  | CTypeDef (id, typ) -> cmd, add_alias_binding id (resolve_type typ dta) dta

let remove_aliases cmd = fst @@ resolve_cmd cmd StringMap.empty
>>>>>>> Implement TLin

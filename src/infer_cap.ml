open Ast
open Ast_visitor

type context = (expr * id) list [@@deriving show]

(** Generate unique names of capability commands *)
(** TODO(rachit): This implementation is wrong because it can cause conflicts
 * with pre-existing id`s. Use a Trie implementation to fix conflicts. *)
module type Gen_name = sig
  val fresh : id -> id
end

module Make () : Gen_name = struct
  module ST = Core.String.Table
  let map = ST.create ()
  let fresh id = match ST.find map id with
    | Some v -> ST.incr map id; id ^ string_of_int (v + 1)
    | None -> ST.add map id 0; id ^ "0"
end


module GN = Make ()

(** This visitor renames all array accesses in read positions with a capability
 * and adds the capability statements. *)
class infer_read_capabilities = object (self)
  (** We carry two context list for each command. The second context represents
   * all the capability that haven't been inserted into the code while the first
   * context contains alls the bindings. *)
  inherit [(context * context)] ast_mapper as super

  method private array_access_helper name exp st =
    match List.assoc_opt exp (fst st) with
      | Some id -> EVar id, st
      | None ->
          let id = GN.fresh name in
          EVar id, (fst st, (exp, id) :: (snd st))

  (** For an array access, first recur into the access expressions and generate
   * the context. Next bind this array access to a capability name. *)
  method! private eaa (name, es) st =
    let es', st1 = self#elist_visit es st in
    let exp = EAA (name, es') in
    self#array_access_helper name exp st1

  method! private ebankedaa (name, e1, e2) st =
    let e1', st1 = self#expr e1 st in
    let e2', st2 = self#expr e2 st1 in
    let exp = EBankedAA (name, e1', e2') in
    self#array_access_helper name exp st2

  (** Don't recur into CCap *)
  method! private ccap (cap, e, id) st = match cap with
    | Read -> CCap (cap, e, id) , ((e, id) :: (fst st), snd st)
    | Write -> CCap (cap, e, id) , st

  (** Ignore LVal in reassign *)
  method! private creassign (e1, e2) st =
    let e2', st' = self#expr e2 st in
    CReassign (e1, e2'), st'

  (** For each command, we first computer the expressions * capability name.
   * Then, we add the capability statements before the command. *)
  method! command c st =
    let to_cap (e, id) = CCap (Read, e, id) in
    let c', st' = super#command c st in
    if List.length (snd st') > 0 then (** Don't add CSeq if there are no capabilities to add. *)
      let caps = List.map to_cap (snd st') in     (** Generate read capabilities. *)
      CSeq (List.rev @@ c' :: caps), (snd st' @ fst st', []) (** Reverse to account for bottom-up. *)
    else
      c', st'

  (** Don't recur into type_nodes *)
  method! type_node t st = t, st

end

(** This visitor renames all array accesses in write positions with a capability
 * and adds the capability statements. *)
class infer_write_capabilities = object
  inherit [context] ast_mapper as super

  (** Don't recur into the expression because we don't want to modify it. *)
  method! private ccap (cap, e, id) st = match cap with
    | Write -> CCap (cap, e, id) , ((e, id) :: st)
    | Read -> CCap (cap, e, id) , st

  method! private creassign (e1, e2) st = match (List.assoc_opt e1 st, e1) with
    | Some id, _ -> super#creassign (EVar id, e2) st
    | None, EAA (id, _) | None, EBankedAA (id, _, _) ->
        let id1 = GN.fresh id in
        let cap = CCap (Write, e1, id1) in
        let e2', st' = super#expr e2 st in
        CSeq [cap; CReassign (EVar id1, e2')], ((e1, id1) :: st')
    | None, _ -> super#creassign (e1, e2) st

  (** Don't recur into type_nodes *)
  method! type_node t st = t, st

  (** Don't recur into expressions *)
  method! expr e st = e, st
end

type inv_context = (id * expr) list

(** This class is the inverse of the infer_* classes. It replaces the capability
 * names with the respective array access. *)
class readd_capabilities = object
  inherit [inv_context] ast_mapper

  method! evar id st = match List.assoc_opt id st with
    | Some e -> e, st
    | None -> EVar id, st

end

let infer_cap cmd =
  let cmd1, (ctx1, _) = (new infer_read_capabilities)#command cmd ([], []) in
  let cmd2, ctx2 = (new infer_write_capabilities)#command cmd1 [] in
  cmd2, ctx2 @ ctx1

let readd_cap cmd (ctx : context) =
  List.map (fun (a, b) -> b, a) ctx |> (new readd_capabilities)#command cmd |> fst

open Ast
open Ast_visitor

type context = (expr * id) list

(** Generate unique names of capability commands *)
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

class infer_read_capabilities = object (self)
  inherit [context] ast_mapper as super

  (** For an array access, first recur into the access expressions and generate
   * the context. Next bind this array access to a capability name. *)
  method! private eaa (name, es) st =
    let es', st1 = self#elist_visit es st in
    let exp = EAA (name, es') in
    match List.assoc_opt exp st1 with
      | Some id -> EVar id, st1
      | None ->
          let id = GN.fresh name in
          EVar id, (exp, id) :: st1

  method! private ebankedaa (name, e1, e2) st =
    let e1', st1 = self#expr e1 st in
    let e2', st2 = self#expr e2 st1 in
    let exp = EBankedAA (name, e1', e2') in
    match List.assoc_opt exp st2 with
      | Some id -> EVar id, st2
      | None ->
          let id = GN.fresh name in
          EVar id, (exp, id) :: st2

  (** Don't recur into CCap *)
  method! private ccap (cap, e, id) st =
    CCap (cap, e, id) , ((e, id) :: st)
  (** Ignore LVal in reassign *)
  method! private creassign (e1, e2) st =
    let e2', st' = self#expr e2 st in
    CReassign (e1, e2'), st'

  (** For each command, we first computer the expressions * capability name.
   * Then, we add the capability statements before the command. *)
  method! command c st =
    let to_cap (e, id) = CCap (Read, e, id) in
    let c', st' = super#command c st in   (** Start with the empty context *)
    let caps = List.map to_cap st' in     (** Generate read capabilities. *)
    CSeq (List.rev @@ c' :: caps), st     (** Reverse to account for bottom-up. *)

  (** Don't recur into type_nodes *)
  method! type_node t st = t, st

end

class infer_write_capabilities = object
  inherit [context] ast_mapper as super

  (** Don't recur into the expression because we don't want to modify it. *)
  method! private ccap (cap, e, id) st =
    CCap (cap, e, id) , ((e, id) :: st)

  method! private creassign (e1, e2) st = match (List.assoc_opt e1 st, e1) with
    | Some id, _ -> super#creassign (EVar id, e2) st
    | None, EAA (id, _) | None, EBankedAA (id, _, _) ->
        let id1 = GN.fresh id in
        let cap = CCap (Write, e1, id1) in
        super#cseq [cap; CReassign (EVar id1, e2)] @@ (e1, id1) :: st
    | None, _ -> super#creassign (e1, e2) st

  (** Don't recur into type_nodes *)
  method! type_node t st = t, st

  (** Don't recur into expressions *)
  method! expr e st = e, st
end

let infer_cap cmd =
  let cmd1, ctx = (new infer_read_capabilities)#command cmd [] in
  (new infer_write_capabilities)#command cmd1 ctx

open Ast

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

type context = (expr * capability) list

let infer_seq infer_f seq ctx =
  List.fold_left (fun acc x -> acc @ x) [] @@
  List.map (fun e -> infer_f e ctx) seq

let rec infer_expr e ctx : context = match e with
  | EInt _ | EFloat _ | EVar _ | EBool _ -> ctx
  | EBinop (_, e1, e2) -> (infer_expr e1 ctx) @ (infer_expr e2 ctx)
  | EBankedAA (_, e1, e2) ->
      (e, Read) :: (infer_expr e1 ctx) @ (infer_expr e2 ctx)
  | EAA (_, es) ->
      List.fold_left (fun acc x -> acc @ x) [(e, Read)] @@
        List.map (fun e -> infer_expr e ctx) es

let rec infer_cmd cmd ctx = match cmd with
  | CMuxDef _ | CTypeDef _ | CCap _ -> ctx
  | CAssign (_, e) -> infer_expr e ctx
  | CFor (_, e1, e2, _, c) ->
      (infer_expr e1 ctx) @ (infer_expr e2 ctx) @ (infer_cmd c ctx)
  | CReassign (EAA _ as e, e1) | CReassign (EBankedAA _ as e, e1) ->
      (e, Write) :: (infer_expr e1 ctx)
  | CReassign (e1, e2) -> (infer_expr e1 ctx) @ (infer_expr e2 ctx)
  | CIf (e, c) -> (infer_expr e ctx) @ (infer_cmd c ctx)
  | CSeq es -> infer_seq infer_cmd es ctx
  | CFuncDef (_, _, c) -> infer_cmd c ctx
  | CApp (_, es) -> infer_seq infer_expr es ctx
  | CExpr e -> infer_expr e ctx

let generate_cap_map cmd : (id * expr * capability) list =
  let ctx = infer_cmd cmd [] in
  let module GN = Make () in
  let generate_name (e, cap) = match e with
    | EBankedAA (id, _, _) | EAA (id, _) -> (GN.fresh id, e, cap)
    | _ -> failwith
      ("Impossible: Capabilities list should not contain" ^ show_expr e)
  in
  List.map generate_name ctx

let generate_cap_seq seq : command =
  CSeq (List.map (fun (id, exp, cap) -> CCap (cap, exp, id)) seq)

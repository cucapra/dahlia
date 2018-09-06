open Ast

let make_assignment id expr =
  CAssign (id, expr)

let make_int x =
  EInt (x, true)

let make_float f =
  EFloat f

let make_bool b =
  EBool (b)

let make_binop bop e1 e2 =
  EBinop (bop, e1, e2)

let make_for x x1 x2 e =
  CFor (x, x1, x2, None, e)

let make_for_impl x x1 x2 u body =
  CFor (x, x1, x2, Some u, body)

let make_var id =
  EVar id

let make_reassignment e1 e2 =
  CReassign (e1, e2)

let make_if b body =
  CIf (b, body)

let make_seq c1 c2 =
  CSeq (c1, c2)

let make_banked_aa id e1 e2 =
  EBankedAA (id, e1, e2)

let make_aa id idx_exprs =
  EAA (id, idx_exprs)

let make_function id args body =
  CFuncDef (id, args, body)

let make_app f a =
  CApp (f, a)

let make_typedef tname tval =
  CTypeDef (tname, tval)

let make_muxdef s mid aid =
  CMuxDef (mid, aid, s)

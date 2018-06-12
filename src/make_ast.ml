open Ast

let make_assignment id expr =
  CAssignment (id, expr)

let make_int x =
  EInt (x)

let make_bool b =
  EBool (b)

let make_binop bop e1 e2 =
  EBinop (bop, e1, e2)

let make_for x x1 x2 e =
  CFor (x, x1, x2, e)

let make_var id =
  EVar id

let make_array s b t =
  match t with
  | TInt -> EArray (TInt, b, (Array.make s (EInt 0)))
  | TBool -> EArray (TBool, b, (Array.make s (EBool false)))

let make_reassignment e1 e2 =
  CReassign (e1, e2)

let make_if b body =
  CIf (b, body)

let make_array_access id index =
  EArrayAccess (id, index)

let make_seq c1 c2 =
  CSeq (c1, c2)

let make_array_access_expl id e1 e2 =
  EArrayExplAccess (id, e1, e2)

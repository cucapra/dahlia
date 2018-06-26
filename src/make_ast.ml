open Ast

let make_assignment id expr =
  CAssign (id, expr)

let make_int x =
  EInt (x, true)

let make_bool b =
  EBool (b)

let make_binop bop e1 e2 =
  EBinop (bop, e1, e2)

let make_for x x1 x2 e =
  CFor (x, x1, x2, e)

let make_for_impl x x1 x2 u body =
  CForImpl (x, x1, x2, u, body)

let make_var id =
  EVar id

let make_array s b t =
  match t with
  | TInt _ -> EArray (TInt None, b, (Array.make s (EInt (0, true))))
  | TBool -> EArray (TBool, b, (Array.make s (EBool false)))
  | _ -> failwith "Implement rest of array types"

let make_reassignment e1 e2 =
  CReassign (e1, e2)

let make_if b body =
  CIf (b, body)

let make_seq c1 c2 =
  CSeq (c1, c2)

let make_array_access_expl id e1 e2 =
  EArrayExplAccess (id, e1, e2)

let make_array_access_impl id e1  =
  EArrayImplAccess (id, e1)

let make_function t id args body =
  CFun (t, id, args, body)

let make_return e =
  CReturn e

let make_app f a =
  EApp (f, a)

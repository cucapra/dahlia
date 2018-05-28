open Ast

let make_assignment id expr =
  CAssignment (id, expr)

let make_int x =
  EInt (x)

let make_bool b =
  EBool (b)

let make_binop bop e1 e2 =
  EBinop (bop, e1, e2)

let make_range x1 x2 =
  ERange (x1, x2)


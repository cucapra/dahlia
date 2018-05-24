open Ast

let make_assignment id expr =
  Assignment (id, expr)

let make_int x =
  Int (x)



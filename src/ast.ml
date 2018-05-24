type id = string

type expression =
  | Int of int
  | Var of id

type command =
  | Assignment of id * expression

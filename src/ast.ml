type id = string

type binop =
  | BopEq
  | BopNeq
  | BopGeq
  | BopLeq
  | BopLt
  | BopGt

type expression =
  | EInt of int
  | EVar of id
  | EBinop of binop * expression * expression
  | EBool of bool
  
type command =
  | Assignment of id * expression

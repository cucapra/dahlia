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
  | EBool of bool
  | EBinop of binop * expression * expression
  | ERange of int * int (* TODO: expressions instead of ints? *)

type value =
  | VInt of int
  | VBool of bool
  | VRange of int * int
  
type command =
  | CAssignment of id * expression

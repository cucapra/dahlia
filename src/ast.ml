type id = string

type binop =
  | BopEq
  | BopNeq
  | BopGeq
  | BopLeq
  | BopLt
  | BopGt
  | BopPlus
  | BopMinus 
  | BopTimes

type expression =
  | EInt of int
  | EVar of id
  | EBool of bool
  | EBinop of binop * expression * expression

type value =
  | VInt of int
  | VBool of bool
  | VRange of int * int
  
type command =
  | CAssignment of id * expression
  | CFor of id * int * int * command list

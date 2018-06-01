type id = string

type type_annotation =
  | AInt
  | ABool

type type_node =
  | TInt
  | TBool
  | TArray

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
  | EArray of expression array

type value =
  | VInt of int
  | VBool of bool
  | VRange of int * int
  | VArray of value array
  
type command =
  | CAssignment of id * expression
  | CFor of id * int * int * command list
  | CArrayUpdate of id * expression * expression
  | CIf of expression * command list

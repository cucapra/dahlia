type id = string

type type_node =
  | TInt of int option
  | TBool
  | TArray of type_node * int
  | TIndex of int

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
  | BopAnd
  | BopOr

type expression =
  | EInt of int * bool
  | EIndex of int
  | EVar of id
  | EBool of bool
  | EBinop of binop * expression * expression
  | EArray of type_node * int * expression array
  | EArrayExplAccess of id * expression * expression
  | EArrayImplAccess of id * expression

type value =
  | VInt of int
  | VBool of bool
  | VRange of int * int
  | VArray of value array
  | VIndex of int
  
type command =
  | CAssignment of id * expression
  | CFor of id * expression * expression * command
  | CForImpl of id * expression * expression * command
  | CReassign of expression * expression
  | CIf of expression * command
  | CSeq of command * command

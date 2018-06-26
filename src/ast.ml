type id = string

type type_node =
  | TInt of int option (* Some i => static int with value i *)
  | TBool
  | TArray of type_node * int (* (type, banking factor) *)
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
  | EApp of id * expression list

type value =
  | VInt of int
  | VBool of bool
  | VRange of int * int
  | VArray of value array
  | VIndex of int
  
type command =
  | CAssign of id * expression
  | CFor of id * expression * expression * command
  | CForImpl of id * expression * expression * expression * command
  | CReassign of expression * expression
  | CIf of expression * command
  | CSeq of command * command
  | CFun of type_node * id * expression list * command
  | CReturn of expression

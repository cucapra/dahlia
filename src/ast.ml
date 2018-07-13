type id = string

type type_node =
  | TInt of int option (* Some i => static int with value i *)
  | TBool
  | TArray of type_node * int * int (* (type, banking factor, size) *)
  | TIndex of int list
  | TFunc of type_node list (* list of arg types *)
  | TAlias of id
  | TFloat

type binop =
  | BopEq
  | BopNeq
  | BopGeq
  | BopLeq
  | BopLt
  | BopGt
  | BopPlus
  | BopMinus
  | BopAnd
  | BopTimes
  | BopOr

type expression =
  | EInt of int * bool
  | EFloat of float
  | EIndex of int list
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
  | CAssign of id * expression
  | CFor of id * expression * expression * command
  | CForImpl of id * expression * expression * int * command
  | CForDouble of id * id * expression * expression * int * command
  | CReassign of expression * expression
  | CIf of expression * command
  | CSeq of command * command
  | CFuncDef of id * (id * type_node) list * command
  | CTypeDef of id * type_node
  | CApp of id * (expression list)

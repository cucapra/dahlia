type id = string

(* A [type_node] is one of the following: 
     - [TInt i]: an integer type, where if [i] is [Some i'], it's a 
       static integer with value i', and if [i] is [None], it's non-static
     - [TBool]: a boolean type
     - [TArray (t, bf, s)]: an array type with array element type [t], 
       banking factor [bf], and size [s]
     - [TIndex (s, d)]: an index type where [s] is its static component, 
       a list of integers representing the indices it itself simultaneously
       represents; and where [d] is its dynamic component, which is [None] 
       if it has no dynamic component, and [Some s] where s is the maximum 
       value it can dynamically represent
     - [TFunc t]: a function type where [t] is a list of the types of its 
       arguments (the order of the list corresponds to the order of the 
       arguments)
     - [TAlias i]: an alias type where [i] is an id; under a typing context 
       [c] this may or may not map to another [type_node]
     - [TFloat]: a float type *)       
type type_node =
  | TInt of int option 
  | TBool
  | TArray of type_node * int * int 
  | TIndex of int list * int option 
  | TFunc of type_node list 
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
  | EPhysAccess of id * expression * expression
  | ELoglAccess of id * expression list

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
  | CReassign of expression * expression
  | CIf of expression * command
  | CSeq of command * command
  | CFuncDef of id * (id * type_node) list * command
  | CTypeDef of id * type_node
  | CApp of id * (expression list)

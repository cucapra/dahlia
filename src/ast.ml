type id = string

(* A [type_node] is one of the following:
 *   - [TBool]: a boolean type
 *   - [TArray (t, [(s1, bf1)..(sn, bfn)] is an array type with elements of
 *     type [t]. It has [n] dimensions, each of size [sn] and banking factor [bfn]
 *   - [TIndex ((l_s, h_s), (l_d, h_d)]: an index type with static component
 *     [l_s]..[h_s] and dynamic component [l_d]..[h_d] (these are non-inclusive ranges).
 *     Index types generalize ints and static ints:
 *       + A static integer with value [n] would be represented as having type
 *         TIndex ((n, n+1), (0, 1))
 *       + A normal old dynamic integer would have type
 *         TIndex ((0, 1) (min_int, max_int))
 *   - [TFunc t]: a function type where [t] is a list of the types of its
 *     arguments (the order of the list corresponds to the order of the
 *     arguments)
 *   - [TAlias i]: an alias type where [i] is an id; under a typing context
 *     [c] this may or may not map to another [type_node]
 *   - [TFloat]: a float type
 *   - [TMux (id, s) a mux type that encapsulates a memory with id [id] and
 *     this mux (not memory) is of size [s] *)
type type_node =
  | TBool
  | TArray of type_node * (int * int) list
  | TIndex of (int * int) * (int * int)
  | TFunc of type_node list
  | TAlias of id
  | TFloat
  | TMux of id * int

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

(* An [expression] is one of the following:
 *   - [EInt (i, s)]: an integer expression with value [i];
 *     if [s] is [true], it's a static integer; otherwise, it's
 *     not.
 *   - [EFloat f]: a float with value [f]
 *   - [EVar i]: a variable with id [i]
 *   - [EBool b]: a boolean with truth value [b]
 *   - [EBinop (b, e1, e2)]: a binop [b] between two expressions
 *     [e1] and [e2]
 *   - [EAA (id, [e1, ..., en])]: an array access where [id] is being
 *     accessed with expressions [e1], ..., [en], where [n] is the number
 *     of array dimensions implied by the access (should match the dimension
 *     of [id] in order to typecheck).
 *   - [EBankedAA (id, exp1, exp2)]: an array access where [exp1] is the bank,
 *     and [exp2] is an index into that bank. [exp1] should be a static int
 *     for a program with such an access to typecheck.
 * Expressions carry information necessary for the compiler to
 * generate C code. *)
type expression =
  | EInt of int * bool
  | EFloat of float
  | EVar of id
  | EBool of bool
  | EBinop of binop * expression * expression
  | EAA of id * expression list
  | EBankedAA of id * expression * expression

(* A [command] is one of the following:
 *   - [CAssign (i, e)]: a representation of assignment of
 *     expression [e] to id [i]
 *   - [CFor (i, x1, x2, b)]: a representation of a for loop
 *     with counter variable [i], range [x1..x2], and body [b]
 *   - [CForImpl (i, x1, x2, u, b)]: a representation of an
 *     unrolled loop with counter variable [i], range [x1..x2],
 *     unroll factor [u] and body [b]
 *   - [CReassign (t, e)]: a representation of a reassignment
 *     of target [t] to expression [e]
 *   - [CIf (e, b)]: a representation of an if statement with
 *     conditional [e] and body [b]
 *   - [CSeq (c1, c2)]: a representation of the command [c1]
 *     followed by [c2]
 *   - [CFuncDef, CTypeDef, CMuxDef, CApp]: TODO *)
type command =
  | CAssign of id * expression
  | CFor of id * expression * expression * command
  | CForImpl of id * expression * expression * int * command
  | CReassign of expression * expression
  | CIf of expression * command
  | CSeq of command * command
  | CFuncDef of id * (id * type_node) list * command
  | CTypeDef of id * type_node
  | CMuxDef of id * id * int
  | CApp of id * expression list

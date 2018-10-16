open Ast

exception IllegalOperation

(* [compute_n a b op] is the result of evaluating the binary
 * operation [op] on [a] and [b]. Helper for [resolve_index_op].
 * Raises [Failure _] if [op] is not some arithmetic binop. *)
let compute_n (a: int) (b: int) (op: binop) =
  match op with
  | BopPlus  -> a + b
  | BopMinus -> a - b
  | BopTimes -> a * b
  | _        ->
    failwith "Impossible case. Boolean operations handled in op_map."

(* [resolve_index_op t1 t2 op] is [TIndex ((ls, hs), (ld, hd)),
 * where:
 *  - [ld] is the lesser of the dynamic component lower bounds
 *    of [t1] and [t2]
 *  - [hd] is the greater of the dynamic component upper bounds
 *    of [t1] and [t2]
 *  - (ls, hs) is (n, n+1), where n = (op) ls1 ls2
 * Raises [IllegalOperation] if [t1] or [t2] have non-trivial
 * static components, i.e. something other than (i, i+1) for
 * some i. *)
let resolve_index_op t1 t2 op =
  match t1, t2 with
  | ((ls1, hs1), (ld1, hd1)), ((ls2, hs2), (ld2, hd2)) ->
    if
      hs1 - ls1 = 1 (* t1 represents one int *) &&
      hs2 - ls2 = 1 (* t2 represents one int *)
    then
      let n  = compute_n ls1 ls2 op in
      let ld = min ld1 ld2 in
      let hd = max hd1 hd2 in
      TIndex ((n, n+1), (ld, hd))
    else raise IllegalOperation

let op_map a b op =
  match a, b, op with
  | _, _, BopEq                          -> TBool
  | _, _, BopNeq                         -> TBool
  | _, _, BopGeq                         -> TBool
  | _, _, BopLeq                         -> TBool
  | _, _, BopLt                          -> TBool
  | _, _, BopGt                          -> TBool
  | TFloat, TFloat, BopPlus              -> TFloat
  | TFloat, TFloat, BopMinus             -> TFloat
  | TFloat, TFloat, BopTimes             -> TFloat
  | _, _, BopAnd                         -> TBool
  | _, _, BopOr                          -> TBool
  | TIndex (s1, d1), TIndex (s2, d2), op -> resolve_index_op (s1, d1) (s2, d2) op
  | _                                    -> raise IllegalOperation

let type_of_op a b op = op_map a b op

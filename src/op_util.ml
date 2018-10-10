open Ast

exception IllegalOperation

let min_int32 = Core.Int.pow (-2) 31

let max_int32 = (Core.Int.pow 2 31) - 1

(* Index types generalize integers. We have not completely figured
   out operations between index types; but we'd like to allow normal
   integer operations between integers. [resolve_index_op t1 t2]
   allows operations between index types that represent *one* value
   at a particular moment, so the static component has cardinality one.
   The resulting type is currently set to be a signed dynamic integer of
   type idx<0..1, min_int..max_int>. This lets us add dynamic integers,
   static integers, etc., but disallows operations between sets of values. *)
let resolve_index_op t1 t2 =
  match t1, t2 with
  | ((ls1, hs1), _), ((ls2, hs2), _) ->
    if
      hs1 - ls1 = 1 (* t1 represents one int *) &&
      hs2 - ls2 = 1 (* t2 represents one int *)
    then TIndex ((ls1, hs1), (min_int, max_int))
    else raise IllegalOperation

let op_map a b op =
  match a, b, op with
  | _, _, BopEq                         -> TBool
  | _, _, BopNeq                        -> TBool
  | _, _, BopGeq                        -> TBool
  | _, _, BopLeq                        -> TBool
  | _, _, BopLt                         -> TBool
  | _, _, BopGt                         -> TBool
  | TFloat, TFloat, BopPlus             -> TFloat
  | TFloat, TFloat, BopMinus            -> TFloat
  | TFloat, TFloat, BopTimes            -> TFloat
  | _, _, BopAnd                        -> TBool
  | _, _, BopOr                         -> TBool
  | TIndex (s1, d1), TIndex (s2, d2), _ -> resolve_index_op (s1, d1) (s2, d2)
  | _                                   -> raise IllegalOperation

let type_of_op a b op = op_map a b op

open Ast
open Context

exception IllegalOperation

let op_map a b op =
  match a, b, op with
  | _, _, BopEq                            -> TBool
  | _, _, BopNeq                           -> TBool
  | _, _, BopGeq                           -> TBool
  | _, _, BopLeq                           -> TBool
  | _, _, BopLt                            -> TBool
  | _, _, BopGt                            -> TBool
  | (TInt _), (TInt _), BopPlus            -> TInt None
  | (TInt _), TFloat, BopPlus              -> TFloat
  | TFloat, (TInt _), BopPlus              -> TFloat
  | TFloat, TFloat, BopPlus                -> TFloat
  | TFloat, (TInt _), BopMinus             -> TFloat
  | (TInt _), (TInt _), BopMinus           -> TInt None
  | (TInt _), TFloat, BopMinus             -> TFloat
  | TFloat, TFloat, BopMinus               -> TFloat
  | TFloat, (TInt _), BopTimes             -> TFloat
  | (TInt _), (TInt _), BopTimes           -> TInt None
  | (TInt _), TFloat, BopTimes             -> TFloat
  | TFloat, TFloat, BopTimes               -> TFloat
  | _, _, BopAnd                           -> TBool
  | _, _, BopOr                            -> TBool
  | TIndex (s, d), TInt _, BopPlus         -> TIndex (s, d)
  | TInt _, TIndex (s, d), BopPlus         -> TIndex (s, d)
  | TIndex (s, d), TInt _, BopMinus        -> TIndex (s, d)
  | TInt _, TIndex (s, d), BopMinus        -> TIndex (s, d)
  | TIndex (s, d), TInt (Some i), BopTimes -> TIndex (List.map (( * ) i) s, d)
  | TInt (Some i), TIndex (s, d), BopTimes -> TIndex (List.map (( * ) i) s, d)
  | _ -> raise IllegalOperation


let rec determine_type t d =
  match t with
  | TAlias id -> determine_type (Context.get_alias_binding id d) d
  | t -> t

let type_of_op a b d op =
  let a_type = determine_type a d in
  let b_type = determine_type b d in
  op_map a_type b_type op

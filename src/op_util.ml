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
  | TFloat, TFloat, BopPlus                -> TFloat
  | TFloat, TFloat, BopMinus               -> TFloat
  | TFloat, TFloat, BopTimes               -> TFloat
  | _, _, BopAnd                           -> TBool
  | _, _, BopOr                            -> TBool
  | _ -> raise IllegalOperation

let rec determine_type t d =
  match t with
  | TAlias id -> determine_type (Context.get_alias_binding id d) d
  | t -> t

let type_of_op a b d op =
  let a_type = determine_type a d in
  let b_type = determine_type b d in
  op_map a_type b_type op

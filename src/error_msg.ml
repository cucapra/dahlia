open Ast

let rec string_of_type = function
  | TBool -> "bool"
  | TArray (t, _) -> (string_of_type t) ^ " array"
  | TIndex (s, d) -> "index"
  | TAlias id -> id
  | TFloat -> "float"
  | TFunc _ -> "func"

let string_of_binop = function
  | BopEq    -> "="
  | BopNeq   -> "!="
  | BopGeq   -> ">="
  | BopLeq   -> "<="
  | BopLt    -> "<"
  | BopGt    -> ">"
  | BopPlus  -> "+"
  | BopMinus -> "-"
  | BopTimes -> "*"
  | BopOr    -> "||"
  | BopAnd   -> "&&"

let illegal_bank i id =
  "[Type error] memory \"" ^ id ^ "\" illegal access: bank " ^ string_of_int i

let range_error =
  "[Type error] range start/end must be integers"

let unexpected_type id actual expected =
  "[Type error] " ^ id ^
  " was of type " ^ (string_of_type actual) ^
  " but type " ^ (string_of_type expected) ^ " was expected"

let illegal_accessor_type t id =
  "[Type error] can't access array " ^ id ^ " with type " ^ (string_of_type t)

let illegal_access id =
  "[Type error] can't index into non-array " ^ id

let illegal_op binop t1 t2 =
  "[Type error] can't apply operator '" ^
  (string_of_binop binop) ^ 
  "' to " ^ 
  (string_of_type t1) ^ 
  " and " ^ 
  (string_of_type t2)

let small_mux =
  "[Type error] illegal access operation: mux is smaller than array banking factor"

let illegal_app id =
  "[Type error] " ^ id ^ " is not a function and cannot be applied"

let illegal_mux =
  "[Type error] can't use multiplexer to access non-array"

let static_bank_error =
  "[Type error] Bank accessor must be static"

let improper_unroll =
  "[Type Error] unroll factor must be a multiple of banking factor"

let range_static_error =
  "[Type Error] range start/end must be static"

let incorrect_aa_dims aname expected actual =
  let e_dim_end = if expected = 1 then "" else "s" in
  let a_dim_end = if actual   = 1 then "" else "s" in
  "[Type Error] array \"" ^ aname ^ "\" has " ^ (string_of_int expected) ^
  " dimension" ^ e_dim_end ^ "; attempted array access implies " ^ 
  (string_of_int actual) ^ " dimension" ^ a_dim_end

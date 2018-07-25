open Ast

let rec string_of_type = function
  | TBool -> "bool"
  | TInt _ -> "int"
  | TArray (t, _, _) -> (string_of_type t) ^ " array"
  | TIndex (s, d) ->
    begin
      match d with
      | None -> "index with completely static information"
      | Some _ -> "index with static and dynamic information"
    end
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
  "Type error: Illegal bank access on array " ^ id ^ ": " ^ string_of_int i

let range_error =
  "Type error: Range start/end must be integers"

let unexpected_type id actual expected =
  "Type error: " ^ id ^
  " was of type " ^ (string_of_type actual) ^
  " but type " ^ (string_of_type expected) ^ " was expected"

let illegal_accessor_type t id =
  "Type error: can't access array " ^ id ^ " with type " ^ (string_of_type t)

let illegal_access id =
  "Type error: can't index into non-array " ^ id

let illegal_op binop t1 t2 =
  "Type error: can't apply operator '" ^
  (string_of_binop binop) ^ 
  "' to " ^ 
  (string_of_type t1) ^ 
  " and " ^ 
  (string_of_type t2)

let small_mux =
  "Illegal access operation: mux is smaller than array banking factor"

let illegal_app id =
  id ^ " is not a function and cannot be applied"

let illegal_mux =
  "Type error: can't use multiplexer to access non-array"

let static_bank_error =
  "Bank accessor must be static"


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

let illegal_bank i id =
  "Illegal bank access on array " ^ id ^ ": " ^ string_of_int i

let range_error =
  "Range start/end must be integers"

let unexpected_type id actual expected =
  "Type Error: " ^ id ^
  " was of type " ^ (string_of_type actual) ^
  " but type " ^ expected ^ " was expected"


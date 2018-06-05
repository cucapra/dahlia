open Ast

(* FIXME: this is defined in type.ml too, maybe make some string util module? *)
let string_of_binop = function
  | BopEq -> "="
  | BopNeq -> "!="
  | BopGeq -> ">="
  | BopLeq -> "<="
  | BopLt -> "<"
  | BopGt -> ">"
  | BopPlus -> "+"
  | BopMinus -> "-"
  | BopTimes -> "*"

let rec transpile_exp = function
  | EInt i -> string_of_int i
  | EBool b -> if b then "1" else "0"
  | EVar id -> id
  | EArray arr -> failwith "Implement array transpilation"
  | EArrayAccess (id, idx) -> transpile_array_access id idx
  | EBinop (b, e1, e2) -> transpile_binop b e1 e2
  
and transpile_array_access id idx =
  id ^ "[" ^ (transpile_exp idx) ^ "]"

and transpile_binop b e1 e2 =
  (transpile_exp e1) ^ " " ^ (string_of_binop b) ^ " " ^ (transpile_exp e2)

let rec transpile_cmd = function
  | CFor (id, a, b, body) -> transpile_for id a b body
  | CAssignment (id, exp) -> transpile_assignment id exp
  | CSeq (c1, c2) -> transpile_seq c1 c2
  | CIf (cond, body) -> transpile_if cond body
  | CArrayUpdate (id, idx, exp) -> transpile_array_update id idx exp

and transpile_for id a b body =
  "for (int " ^ id ^ " = " ^ (transpile_exp a) ^ 
  "; " ^ id ^ " < " ^ (transpile_exp b) ^ "; " ^ 
  id ^ " += 1) {" ^ (transpile_cmd body) ^ "}"

(* FIXME: only works for ints. How to get types? From typechecker pass? *)
(* FIXME: also only works for initial assignments *)
and transpile_assignment id exp =
  "int " ^ id ^ " = " ^ (transpile_exp exp) ^ ";"

and transpile_seq c1 c2 =
  (transpile_cmd c1) ^ " " ^ (transpile_cmd c2)

and transpile_if cond body =
  "if (" ^ (transpile_exp cond) ^ ") {" ^ (transpile_cmd body) ^ "}"

and transpile_array_update id idx exp =
  id ^ "[" ^ (transpile_exp idx) ^ "] = " ^ (transpile_exp exp) ^ ";"

let generate_c prog =
  "int main() { " ^ (transpile_cmd prog) ^ " return 0; }"
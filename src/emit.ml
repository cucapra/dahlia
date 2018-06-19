open Ast

let type_map = ref (fun _ -> failwith "TypeMap has not been set")

let set_type_map t =
  type_map := t; ()

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
  | BopAnd -> "&&"
  | BopOr -> "||"

let rec transpile_exp = function
  | EInt (i, _) -> string_of_int i
  | EBool b -> if b then "1" else "0"
  | EVar id -> id
  | EArray _ -> failwith "Implement array transpilation"
  | EBinop (b, e1, e2) -> transpile_binop b e1 e2
  | EArrayExplAccess (id, idx1, idx2) -> transpile_explicit_array_access id idx1 idx2
  | EArrayImplAccess _ -> failwith "Implement implicit array compilation"
  | EIndex _ -> failwith "Implement index compilation"
  
and transpile_binop b e1 e2 =
  (transpile_exp e1) ^ " " ^ (string_of_binop b) ^ " " ^ (transpile_exp e2)

and transpile_cmd = function
  | CFor (id, a, b, body) -> transpile_for id a b body
  | CAssign (id, exp) -> transpile_assignment id exp
  | CSeq (c1, c2) -> transpile_seq c1 c2
  | CIf (cond, body) -> transpile_if cond body
  | CReassign (target, exp) -> transpile_reassign target exp
  | CForImpl _ -> failwith "Implement for loop implicit"

and transpile_for id a b body =
  "for (int " ^ id ^ " = " ^ (transpile_exp a) ^ 
  "; " ^ id ^ " < " ^ (transpile_exp b) ^ "; " ^ 
  id ^ " += 1) {" ^ (transpile_cmd body) ^ "}"

(* FIXME: only works with arrays/ints/bools *)
and transpile_assignment id exp =
  match !type_map id, exp with
  | TInt _, _
  | TBool, _ -> "int " ^ " " ^ id ^ " = " ^ (transpile_exp exp) ^ ";"
  | (TArray (t, _)), EArray (_, s, _) -> "int " ^ id ^ "[" ^ (string_of_int s) ^ "];"
  | _ -> failwith "Impossible"

and transpile_seq c1 c2 =
  (transpile_cmd c1) ^ " " ^ (transpile_cmd c2)

and transpile_if cond body =
  "if (" ^ (transpile_exp cond) ^ ") {" ^ (transpile_cmd body) ^ "}"

and transpile_explicit_array_access id b i =
  match !type_map id with
  | (TArray (_, bf)) -> id ^ "[" ^ (transpile_exp b) ^ "+" ^ (string_of_int bf) ^ "*" ^ (transpile_exp i) ^ "]"
  | failwith -> "Improper type passed in emitter"

and transpile_reassign target exp =
  (match target with
  | EArrayExplAccess (id, b, i) -> transpile_explicit_array_access id b i
  | EVar id -> id
  | _ -> failwith "Impossible")
  |> (fun left -> left ^ " = " ^ (transpile_exp exp) ^ ";") 

let generate_c prog =
  "int main() { " ^ (transpile_cmd prog) ^ " return 0; }"

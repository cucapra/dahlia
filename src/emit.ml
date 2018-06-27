open Ast

let type_map = ref (fun _ -> failwith "TypeMap has not been set")

let set_type_map t =
  type_map := t

let rec indent' n s acc =
  if n=0 then acc
  else indent' (n-1) s (acc ^ "\t")

let indent n s = indent' n s ""

let newline = "\n"

let s_pragma_unroll u =
  "#pragma HLS UNROLL factor=" ^ u

let bop_str = function
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

let concat =
  List.fold_left (fun acc e -> acc ^ e) ""

let rec emit_expr = function
  | EInt (i, _)                 -> emit_int i
  | EBool b                     -> emit_bool b
  | EVar v                      -> emit_var v
  | EArray (_, _, a)            -> emit_array a
  | EBinop (b, e1, e2)          -> emit_binop (b, e1, e2)
  | EArrayExplAccess (id, _, i) -> emit_aa (id, i)
  | EArrayImplAccess (id, i)    -> emit_aa (id, i)
  | EApp (id, args)             -> emit_app (id, args)
  | EIndex _ -> failwith "Implement index stuff"

and emit_int i = string_of_int i

and emit_bool b = if b then "1" else "0"

and emit_var id = id

and emit_array _ = 
  failwith "Typechecker failed to catch literal array access"

and emit_binop (b, e1, e2) = 
  concat [ (emit_expr e1); (bop_str b); (emit_expr e2) ]

and emit_aa (id, i) =
  concat [ id; "["; (emit_expr i); "]" ]

and emit_app (id, args) =
  (fun acc e -> concat [ acc; ", "; (emit_expr e) ])
  |> fun f -> List.fold_left f "" args
  |> fun s -> String.sub s 0 ((String.length s) - 2)

let rec emit_cmd i = function
  | CAssign (id, e)         -> emit_assign (id, e)
  | CFor (id, r1, r2, body) -> emit_for (id, r1, r2, body) i
  | CIf (cond, body)        -> emit_if (cond, body) i
  |> (indent i)

and emit_assign (id, e) =
  concat [ (!type_map id); " "; id; " = "; (emit_expr e) ]

and emit_for (id, r1, r2, body) i =
  concat [ 
    "for (int "; id; " = "; (emit_expr r1); ";"; id;
    " <= "; (emit_expr r2); " "; id; " += 1) {"; newline
  ] 
  |> fun s -> concat [ s; (emit_cmd (i+1) body); newline; "}" ]

and emit_if (cond, body) i =
  concat [ 
    "if ("; (emit_expr cond); ") {"; newline; (emit_cmd (i+1) body);
    newline; "}"
  ]


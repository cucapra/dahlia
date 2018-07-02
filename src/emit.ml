open Ast

let type_map = ref (fun _ -> failwith "TypeMap has not been set")
let delta_map = ref (fun _ -> failwith "DeltaMap has not been set")

let set_type_map t =
  type_map := t

let set_delta_map t =
  delta_map := t

let rec indent' n s acc =
  if n=0 then acc ^ s
  else indent' (n-1) s (acc ^ "\t")

let indent n s = indent' n s ""

let newline = "\n"

let concat =
  List.fold_left (fun acc e -> acc ^ e) ""

let s_pragma_unroll u i =
  concat [ "#pragma HLS UNROLL factor="; u ]
  |> indent i

let s_pragma_bank id bf i =
  concat [ 
    "#pragma HLS ARRAY_PARTITION variable="; id;
    " factor="; (string_of_int bf) 
  ] |> indent i

let rec type_str = function
  | TBool
  | TInt _        -> "int"
  | TIndex _      -> failwith "Implement indices"
  | TArray (t, _) -> failwith "Implement array type stringified version"
  | TAlias id -> type_str (!delta_map id)

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

let rec emit_expr = function
  | EInt (i, _)                 -> emit_int i
  | EBool b                     -> emit_bool b
  | EVar v                      -> emit_var v
  | EArray (_, _, a)            -> emit_array a
  | EBinop (b, e1, e2)          -> emit_binop (b, e1, e2)
  | EArrayExplAccess (id, _, i) -> emit_aa (id, i)
  | EArrayImplAccess (id, i)    -> emit_aa (id, i)
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

and argvals =
  List.map ((fun (id, t) -> 
    match t with
    | TArray _ -> concat [ "*"; id ]
    | _ -> id))

and emit_args args =
  (fun acc e -> concat [ acc; ", "; e ]) |> fun f ->
  List.fold_left f "" args               |> fun s -> 
  String.sub s 2 ((String.length s) - 2)

and emit_anno_args a = emit_args (argvals a)

and emit_app (id, args) i =
  concat [ id; "("; (emit_args (List.map emit_expr args)); ");" ] 
  |> indent i

let rec emit_cmd i cmd =
  match cmd with
  | CAssign (id, e)                -> emit_assign (id, e) i
  | CReassign (target, e)          -> emit_reassign (target, e) i
  | CFor (id, r1, r2, body)        -> emit_for (id, r1, r2, body, None) i
  | CForImpl (id, r1, r2, u, body) -> emit_for (id, r1, r2, body, (Some u)) i
  | CIf (cond, body)               -> emit_if (cond, body) i
  | CSeq (c1, c2)                  -> emit_seq (c1, c2) i
  | CFuncDef (id, args, body)      -> emit_fun (id, args, body) i
  | CApp (id, args)                -> emit_app (id, args) i
  | CTypeDef (id, t)               -> emit_typedef (id, t) i

and emit_assign_int (id, e) =
  concat [ "int "; id; " = "; (emit_expr e); ";" ]

and emit_assign_arr (id, e, a, bf) i =
  let arr_size = string_of_int (Array.length a) in
  concat [
    "int "; id; "["; arr_size; "]"; ";"; newline;
    (s_pragma_bank id bf i)
  ]

and emit_assign (id, e) i =
  match !type_map id, e with
  | TInt _, _
  | TBool, _                         -> emit_assign_int (id, e)          |> indent i
  | TArray (t, bf), EArray (_, _, a) -> emit_assign_arr (id, e, a, bf) i |> indent i

and emit_reassign (target, e) i =
  concat [ (emit_expr target); " = "; (emit_expr e); ";" ] |> indent i

and emit_for (id, r1, r2, body, u) i =
  let unroll_pragma =
    (match u with
     | None -> ""
     | Some u' -> 
       concat [ newline; (s_pragma_unroll (emit_expr u') (i+1)) ]) in
  concat [ 
    "for (int "; id; " = "; (emit_expr r1); "; "; id;
    " <= "; (emit_expr r2); "; "; id; " += 1) {";
    unroll_pragma; newline
  ] 
  |> fun s -> concat [ s; (emit_cmd (i+1) body); newline; (indent i "}") ]
  |> indent i

and emit_if (cond, body) i =
  concat [ 
    "if ("; (emit_expr cond); ") {"; newline; (emit_cmd (i+1) body);
    newline; "}"
  ]
  |> indent i

and emit_seq (c1, c2) i =
  concat [ (emit_cmd i c1); newline; (emit_cmd i c2); ]

and emit_fun (id, args, body) i =
  concat [ 
    "void "; id; "("; (emit_anno_args args); ") {";
    newline; (emit_cmd (i+1) body); newline; (indent i "}")
  ]
  |> indent i

and emit_typedef (id, t) i =
  concat [ "typedef "; (type_str t); " "; id; ";" ]
  |> indent i

and generate_c cmd =
  emit_cmd 0 cmd

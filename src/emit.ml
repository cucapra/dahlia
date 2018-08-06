open Ast

let type_map = ref (fun _ -> failwith "TypeMap has not been set")
let delta_map = ref (fun _ -> failwith "DeltaMap has not been set")

let compute_bf d =
  List.fold_left (fun acc (_, d) -> d * acc) 0 d

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
  if bf=1 then ""
  else
    concat [ 
      "#pragma HLS ARRAY_PARTITION variable="; id;
      " factor="; (string_of_int bf) 
    ] |> indent i

let rec type_str = function
  | TBool
  | TInt _        -> "int"
  | TFloat        -> "float"
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
  | EInt (i, _)            -> emit_int i
  | EFloat f               -> emit_float f
  | EBool b                -> emit_bool b
  | EVar v                 -> emit_var v
  | EArray (_, _, a)       -> emit_array a
  | EBinop (b, e1, e2)     -> emit_binop (b, e1, e2)
  | EPhysAccess (id, b, i) -> emit_aa_phys (id, b, i)
  | ELoglAccess (id, i)    -> emit_aa_logl (id, i)

and emit_int i = string_of_int i

and emit_float f = string_of_float f

and emit_bool b = if b then "1" else "0"

and emit_var id = id

and emit_array _ = 
  failwith "Typechecker failed to catch literal array access"

and emit_binop (b, e1, e2) = 
  concat [ (emit_expr e1); (bop_str b); (emit_expr e2) ]

and determine_bf id =
  match !type_map id with
  | TArray (_, d) -> compute_bf d
  | _ -> 
    failwith "Typechecker failed to determine that mux is illegally wrapped around array"

and emit_aa_phys (id, b, i) =
  match !type_map id with
  | TArray (_, d) ->
    let bf = compute_bf d in 
    concat [ id; "["; (emit_expr b); " + "; (string_of_int bf); "*("; (emit_expr i); ")]" ]
  | TMux (a_id, _) ->
    let bf = determine_bf a_id in
    concat [ id; "["; (emit_expr b); " + "; (string_of_int bf); "*("; (emit_expr i); ")]" ]
  | _ -> failwith "Tried to index into non-array"

(* FIXME: optimize? *)
and flatten_access dims idx_exprs =
  let prod_dims:int = List.fold_left (fun e (_, d) -> d * e) 1 dims in
  match dims, idx_exprs with
  | _::td, hi::ti -> 
    concat [ (string_of_int prod_dims); "*("; (emit_expr hi); ")+"; (flatten_access td ti) ]
  | _ -> failwith "Flatten failed"

and emit_aa_logl (id, idx_exprs) =
  match !type_map id with
  | TArray (_, dims) ->
    concat [ id; "["; (flatten_access dims idx_exprs); "]"; ]
  | _ -> failwith "Tried to index into non-array"

and argvals =
  List.map ((fun (id, t) -> 
    match t with
      | TArray (t, d) -> 
        let s = List.fold_left (fun acc (s, _) -> s * acc) 0 d in
        concat [ (type_str t); " "; id; "["; (string_of_int s); "]" ]
      | t -> concat [ (type_str t); " "; id  ] 
  )) 

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
  | CMuxDef (m_id, a_id, s)        -> emit_muxdef (m_id, a_id, s)

and emit_assign_int (id, e) =
  concat [ "int "; id; " = "; (emit_expr e); ";" ]

and emit_assign_arr (id, e, a, d) i =
  let bf = compute_bf d in
  let arr_size = string_of_int (Array.length a) in
  let part_pragma =
    if bf=1 then ""
    else s_pragma_bank id bf i
  in concat [
    "int "; id; "["; arr_size; "]"; ";"; newline;
    part_pragma
  ]

and emit_assign_float (id, e) =
  concat [ "float "; id; " = "; (emit_expr e); ";" ]

and emit_assign (id, e) i =
  match !type_map id, e with
  | TInt _, _
  | TBool, _                        -> emit_assign_int (id, e)         |> indent i
  | TArray (t, d), EArray (_, _, a) -> emit_assign_arr (id, e, a, d) i |> indent i
  | TFloat, _                       -> emit_assign_float (id, e)       |> indent i

and emit_reassign (target, e) i =
  concat [ (emit_expr target); " = "; (emit_expr e); ";" ] |> indent i

and emit_for (id, r1, r2, body, u) i =
  let unroll_pragma =
    (match u with
     | None -> ""
     | Some u' -> 
       concat [ newline; (s_pragma_unroll (emit_int u') (i+1)) ]) in
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

and emit_pragmas lst i =
  (fun acc elem ->
     match elem with
     | id, TArray (_, d) ->
       let bf = compute_bf d in
       concat [ acc; (s_pragma_bank id bf (i+1)); newline; ]
     | _ -> acc)
  |> fun f -> List.fold_left f "" lst

and emit_fun (id, args, body) i =
  let pragmas = emit_pragmas args i in
  concat [ 
    "void "; id; "("; (emit_anno_args args); ") {";
    newline; pragmas; (emit_cmd (i+1) body); newline; (indent i "}")
  ]
  |> indent i

and emit_typedef (id, t) i =
  concat [ "typedef "; (type_str t); " "; id; ";" ]
  |> indent i

and emit_muxdef (m_id, a_id, s) = "" (* No need to emit anything *)

and generate_c cmd =
  emit_cmd 0 cmd

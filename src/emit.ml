open Ast

(** Used to generate the types for identifiers. *)
let type_map = ref (fun _ -> failwith "TypeMap has not been set")
let set_type_map t =
  type_map := t

let cleanup_output s =
  String.split_on_char '\n' s
  |> List.filter (fun s -> String.length s != 0)
  |> List.fold_left (fun acc s -> acc ^ "\n" ^ s) ""

let compute_bf d =
  List.fold_left (fun acc (_, d) -> d * acc) 1 d

let indent n s =
  List.init n (fun _ -> ()) |> List.fold_left (fun acc _ -> "  " ^ acc) s

let newline = "\n"

let concat = String.concat ""

let comment s = Printf.sprintf "/* %s */" s

let s_pragma_unroll u i =
  if u = "1" then ""
  else concat [ "#pragma HLS UNROLL factor="; u ] |> indent i

let s_pragma_bank id bf i =
  if bf=1 then ""
  else
    concat [
      "#pragma HLS ARRAY_PARTITION variable="; id;
      " factor="; (string_of_int bf)
    ] |> indent i

let compute_array_size dims =
  List.fold_left (fun acc (s, _) -> s * acc) 1 dims

let type_str = function
  | TBool | TIndex _ -> "int"
  | TFloat -> "float"
  | t -> failwith (Printf.sprintf "Cannot emit type %s." (show_type_node t))

let rec emit_expr = function
  | EInt (i, _)          -> string_of_int i
  | EFloat f             -> string_of_float f
  | EBool b              -> if b then "1" else "0"
  | EVar id              -> id
  | EBinop (b, e1, e2)   -> emit_binop (b, e1, e2)
  | EBankedAA (id, b, i) -> emit_aa_phys (id, b, i)
  | EAA (id, i)          -> emit_aa_logl (id, i)

and emit_binop (b, e1, e2) =
  concat [ (emit_expr e1); (string_of_binop b); (emit_expr e2) ]

and emit_aa_phys (id, b, i) =
  match !type_map id with
  | TArray (_, d) ->
    let bf = compute_bf d in
    if bf != 1 then
    concat [ id; "["; (emit_expr b); " + "; (string_of_int bf); "*("; (emit_expr i); ")]" ]
    else
    concat [ id; "["; (emit_expr b); " + "; (emit_expr i); ]
  | TMux _ -> failwith "Muxes not implemented"
  | _ -> failwith "Tried to index into non-array"

(* FIXME: optimize?
 * @tedbauer What kind of optimization were you thinking off? It's generally
 * a good idea to document future optimizations that you don't want to immediately
 * work on. If it's significant, open an issue and point to it. -- @rachitnigam *)
and flatten_access dims idx_exprs =
    match dims, idx_exprs with
    | _::td, hi::ti ->
      let prod_dims = List.fold_left (fun e (d, _) -> d * e) 1 td in
      concat [ (string_of_int prod_dims); "*("; (emit_expr hi); ")+"; (flatten_access td ti) ]
    | [], [] -> ""
    | _ -> failwith "Flatten failed"

and emit_aa_logl (id, idx_exprs) =
  match !type_map id with
  | TArray (_, dims) ->
    let idx = flatten_access dims idx_exprs in
    let idx' = String.sub idx 0 (String.length idx - 1) in
    concat [ id; "["; (idx'); "]"; ]
  | _ -> failwith "Tried to index into non-array"

and argvals =
  List.map (fun (id, t) ->
    match t with
      | TArray (t, d) ->
        let s = List.fold_left (fun acc (s, _) -> s * acc) 1 d in
        concat [ (type_str t); " "; id; "["; (string_of_int s); "]" ]
      | t -> concat [ (type_str t); " "; id  ]
  )

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
  | CCap (cap, e, id)          -> emit_cap (cap, e, id) i
  | CAssign (id, e)            -> emit_assign (id, e) i
  | CReassign (target, e)      -> emit_reassign (target, e) i
  | CFor (id, r1, r2, u, body) -> emit_for (id, r1, r2, body, u) i
  | CIf (cond, body)           -> emit_if (cond, body) i
  | CSeq clist                 -> emit_seq clist i
  | CFuncDef (id, args, body)  -> emit_fun (id, args, body) i
  | CApp (id, args)            -> emit_app (id, args) i
  | CTypeDef _                 -> failwith "CTypeDef should not occur in AST"
  | CMuxDef (_, mid, s)        -> emit_mux mid s i
  | CExpr e                    -> emit_expr e
  | CEmpty                     -> ";"

and emit_cap (cap, e, _) i = match cap with
  | Read -> comment ("cap read: " ^ emit_expr e) |> indent i
  | Write -> comment ("cap write: " ^ emit_expr e) |> indent i

and emit_mux _ = failwith "Muxes not implemented"

and emit_assign_int (id, e) =
  concat [ "int "; id; " = "; (emit_expr e); ";" ]

and emit_assign_arr (id, _, d) i =
  let bf = compute_bf d in
  let arr_size = compute_array_size d in
  let part_pragma =
    if bf=1 then ""
    else s_pragma_bank id bf i
  in concat [
    "int "; id; "["; (string_of_int arr_size); "]"; ";"; newline;
    part_pragma
  ]

and emit_assign_float (id, e) =
  concat [ "float "; id; " = "; (emit_expr e); ";" ]

and emit_assign (id, e) i =
  match !type_map id with
  | TIndex _      -> emit_assign_int (id, e)         |> indent i
  | TBool         -> emit_assign_int (id, e)         |> indent i
  | TArray (_, d) -> emit_assign_arr (id, e, d) i    |> indent i
  | TFloat        -> emit_assign_float (id, e)       |> indent i
  | TAlias _      -> failwith "Impossible: TAlias while emitting"
  | t             -> failwith @@ "NYI: emit_assign with " ^ show_type_node t

and emit_reassign (target, e) i =
  concat [ (emit_expr target); " = "; (emit_expr e); ";" ] |> indent i

and emit_for (id, r1, r2, body, u) i =
  let unroll_pragma =
    concat [ newline; (s_pragma_unroll (string_of_int u) (i+1)) ] in
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

and emit_seq clist i =
  let f acc cmd = concat [ acc; newline; (emit_cmd i cmd) ] in
  List.fold_left f "" clist

and emit_pragmas lst i =
  let f acc elem = match elem with
     | id, TArray (_, d) ->
         let bf = compute_bf d in
         concat [ acc; (s_pragma_bank id bf (i+1)); newline; ]
     | _ -> acc in
  List.fold_left f "" lst

and emit_fun (id, args, body) i =
  let pragmas = emit_pragmas args i in
  concat [
    "void "; id; "("; (emit_anno_args args); ") {";
    newline; pragmas; (emit_cmd (i+1) body); newline; (indent i "}")
  ]
  |> indent i

and generate_c cmd =
  emit_cmd 0 cmd |> cleanup_output

open Ast
open Printf

let rec string_of_type = function
  | TBool -> "bool"
  | TArray (t, _) -> (string_of_type t) ^ " array"
  | TIndex (s, d) ->
    let (ls, hs), (ld, hd) = s, d in
    sprintf "idx<%s..%s, %s..%s>"
      (string_of_int ls) (string_of_int hs) (string_of_int ld) (string_of_int hd)
  | TAlias id -> id
  | TFloat -> "float"
  | TFunc _ -> "func"
  | TLin t -> (string_of_type t) ^ " lin"
  | TMux _ -> "mux"

let illegal_bank i id =
  sprintf "[Type error] Bank %d already consumed for memory `%s'" i id

let access_without_index_type typ =
  sprintf "[Type error] Cannot index into array with %s; expected an index type."
    (string_of_type typ)

let range_error =
  "[Type error] range start/end must be integers"

let unexpected_type id actual expected =
  "[Type error] " ^ id ^
  " was of type " ^ (string_of_type actual) ^
  " but type " ^ (string_of_type expected) ^ " was expected"

let illegal_accessor_type t id =
  "[Type error] can't access array " ^ id ^ " with type " ^ (string_of_type t)

let not_an_array id =
  Printf.sprintf "[Type error] `%s' is not an array." id

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

let reassign_type_mismatch t_lval t_rval =
  "[Type Error] cannot assign value of type `" ^
  (string_of_type t_rval) ^
  "' to L-value of type `" ^
  (string_of_type t_lval) ^ "'"

let incorrect_aa_dims aname expected actual =
  let e_dim_end = if expected = 1 then "" else "s" in
  let a_dim_end = if actual   = 1 then "" else "s" in
  "[Type Error] array `" ^ aname ^ "' has " ^ (string_of_int expected) ^
  " dimension" ^ e_dim_end ^ "; attempted array access implies " ^
  (string_of_int actual) ^ " dimension" ^ a_dim_end

let invalid_array_write id =
  Printf.sprintf "Cannot write into array `%s' without write capability." id

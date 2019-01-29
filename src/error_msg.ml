open Ast
open Printf

exception TypeError of string

(* [is_ubit d] is:
 *  - [true] if [d] is (0, 2^n) for some n
    - [false] otherwise *)
let is_ubit (ld, hd) =
  match (ld, hd) with
  | 0, Exp _ -> true
  | _ -> false

let string_of_offset = function
  | Exp e -> string_of_int e
  | Lin l -> string_of_int l

(* [pprint_idx s d] is a string representation of the index type
 * [TIndex (s, d)), which is:
 *  - unsigned bit<n> if [s] is (0, 1), and [d] is (0..2^n)
 *  - idx<...> otherwise *)
let pprint_idx (s:int*int) (d:int*offset) =
  let (ls, hs), (ld, hd) = s, d in
    if (ls, hs) = (0, 1) && is_ubit (ld, hd) then
      sprintf "unsigned bit<%s>" @@ string_of_offset hd
    else
      sprintf "idx<%s..%s, %s..%s>"
        (string_of_int ls)
        (string_of_int hs)
        (string_of_int ld)
        (string_of_offset hd)

let rec string_of_type = function
  | TBool -> "bool"
  | TArray (t, _) -> (string_of_type t) ^ " array"
  | TIndex (s, d) -> pprint_idx s d
  | TAlias id -> id
  | TFloat -> "float"
  | TFunc _ -> "func"
  | TLin t -> (string_of_type t) ^ " lin"
  | TMux _ -> "mux"

let id_already_bound id =
  sprintf "`%s' already bound in this context. Cannot shadow variables." id

let illegal_bank i id =
  sprintf "[Type error] Bank %d already consumed for memory `%s'" i id

let access_without_index_type typ =
  sprintf "[Type error] Cannot index into array with %s; expected an index type."
    (string_of_type typ)

let range_error t1 t2 =
  sprintf
    "[Type error] Expected range start and end to be integers, received start: %s, end %s."
    (string_of_type t1)
    (string_of_type t2)

let range_static_error t1 t2 =
  sprintf
    "[Type Error] Expected range start and end to be static integers, received start:%s, end:%s."
    (string_of_type t1)
    (string_of_type t2)

let unexpected_type id actual expected =
  sprintf "[Type Error] `%s' was of type %s but type %s was expected."
    id
    (string_of_type actual)
    (string_of_type expected)

let illegal_accessor_type t id =
  sprintf "[Type error] can't access array `%s' with type %s." id (string_of_type t)

let illegal_banked_aa t b id =
  sprintf "[Type error] can't access bank %s of array `%s' with type %s."
    (string_of_int b)
    id
    (string_of_type t)

let not_an_array id =
  sprintf "[Type error] `%s' is not an array." id

let illegal_op binop t1 t2 =
  sprintf "[Type error] can't apply operator %s to %s and %s."
  (string_of_binop binop)
  (string_of_type t1)
  (string_of_type t2)

let illegal_app id =
  sprintf "[Type error] `%s' is not a function and cannot be applied." id

let illegal_mux =
  "[Type error] can't use multiplexer to access non-array"

let static_bank_error =
  "[Type error] Bank accessor must be static"

let improper_unroll =
  "[Type Error] unroll factor must be a multiple of banking factor"

let cap_non_array =
  "[Type Error] Only array expressions are allowed in capability statements."

let reassign_type_mismatch t_lval t_rval =
  sprintf "[Type Error] cannot assign value of type `%s' to L-value of type `%s'."
  (string_of_type t_rval)
  (string_of_type t_lval)

let incorrect_aa_dims aname expected actual =
  let e_dim_end = if expected = 1 then "" else "s" in
  let a_dim_end = if actual   = 1 then "" else "s" in
  "[Type Error] array `" ^ aname ^ "' has " ^ (string_of_int expected) ^
  " dimension" ^ e_dim_end ^ "; attempted array access implies " ^
  (string_of_int actual) ^ " dimension" ^ a_dim_end

let invalid_array_write id =
  sprintf "Cannot write into array `%s' without write capability." id

let reassign_bit_violation s_target s_val =
  sprintf
    "[Type Error] Cannot reassign value of type `%s' to value of type `%s';
     `%s' is represented with less bits than `%s'."
  (string_of_type s_target)
  (string_of_type s_val)
  (string_of_type s_target)
  (string_of_type s_val)

let nonex_type tname =
  sprintf
    "[Type Error] No type definition for `%s'.;"
    tname


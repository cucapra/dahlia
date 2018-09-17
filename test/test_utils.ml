open Core
open Seashell
open Compile_utils

let compile_string prog =
  Printexc.record_backtrace false;
  let ast = parse_with_error prog in
  let rast = Resolve_alias.remove_aliases ast in
  let ctx = typecheck_with_error rast in
  emit_code rast ctx

let compile filename =
  let prog = In_channel.read_all filename in
  compile_string prog

let redirect_error f =
  begin
    try f (); failwith "[[This test should have failed!]]" with
      | Failure msg | Type.TypeError msg -> print_endline msg
      | e -> raise e
  end

let compile_string_with_failure prog =
  let f () = compile_string prog in
  redirect_error f

let compile_with_failure prog =
  let f () = compile prog in
  redirect_error f

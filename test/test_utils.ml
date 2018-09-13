open Core
open Seashell
open Compile_utils

let compile_string prog =
  Printexc.record_backtrace false;
  let ast = parse_with_error prog in
  let (ctx, dta) = typecheck_with_error ast in
  emit_code ast ctx dta

let compile filename =
  let prog = In_channel.read_all filename in
  compile_string prog


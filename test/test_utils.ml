open Core
open Seashell

let compile_string prog = Compile_utils.compile_string prog false

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

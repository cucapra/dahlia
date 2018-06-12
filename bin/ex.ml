open Seashell
open Lexer
open Lexing

open Type

let mode = ref ""
let no_typecheck = ref false

let usage = "usage: " ^ Sys.argv.(0) ^ "[-tr]"

let specs = [
  ("-m", Arg.Set_string mode, ": set mode (t=transp, i=interp, n=nothing)");
  ("-nt", Arg.Set no_typecheck, ": turn off typechecking")
]

let _ =

  Arg.parse
    specs
    (fun s -> raise (Arg.Bad ("Invalid argument " ^ s)))
    usage;

  let lexbuf = Lexing.from_channel stdin in
  let commands = Parser.prog Lexer.token lexbuf in
  try
    if not !no_typecheck then
      ignore (check_cmd commands empty_context)
    else ();

    match String.lowercase_ascii !mode with
    | "i" ->
      let final_env = Eval.eval_command (commands, Eval.empty_env) in
      print_endline (Eval.string_of_env final_env)
    | "t" ->
      print_endline (Emit.generate_c commands)
    | "n" -> ()
    | _ ->
      print_endline "Error: not a mode"
    
  with
    TypeError s -> print_endline s

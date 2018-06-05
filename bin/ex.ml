open Seashell
open Lexer
open Lexing

open Type

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let commands = Parser.prog Lexer.token lexbuf in
  try
    (* ignore (check_cmds commands empty_context); *)
    let final_env = Eval.eval_command (commands, Eval.empty_env) in
    (* print_endline (Eval.string_of_env final_env) *)

    print_endline (Emit.generate_c commands)
    
  with
    TypeError s -> print_endline s
open Seashell
open Lexer
open Lexing

open Type

let rec interpret cmd_list env =
  match cmd_list with
  | h::t -> let env' = Eval.eval_command (h, env) in interpret t env'
  | [] -> env


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let commands = Parser.prog Lexer.token lexbuf in
  try
    ignore (check_cmds commands Type.empty_context);
    let final_env = interpret commands Eval.initial_env in
    print_endline (Eval.string_of_env final_env)
  with
    TypeError s -> print_endline s


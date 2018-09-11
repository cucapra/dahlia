open Lexer
open Lexing
open Context

let ( >>= ) opt f = match opt with
| Some v -> f v
| None -> None

let ( >= ) opt f = match opt with
| Some v -> Some (f v)
| None -> None

let pp_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  "line " ^ string_of_int pos.pos_lnum ^
  ", column " ^ string_of_int (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error prog =
  let lexbuf = Lexing.from_string prog in
  try Parser.prog Lexer.token lexbuf with
  | SyntaxError msg ->
    failwith (msg ^ " Lexing error on" ^ pp_position lexbuf)
  | Parser.Error ->
    failwith ("Syntax error on " ^ pp_position lexbuf)

let typecheck_with_error (ast : Ast.command) : (gamma * delta) option =
  try
    Some (Type.check_cmd ast (Context.empty_gamma, Context.empty_delta))
  with
    Type.TypeError s -> Core.fprintf stderr "%s\n" s; exit(-1)


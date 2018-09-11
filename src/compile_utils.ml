open Lexer
open Lexing

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

let typecheck_with_error (ast : Ast.command) =
  try
    Type.check_cmd ast (Context.empty_gamma, Context.empty_delta)
  with
    Type.TypeError s -> failwith s


let emit_code ast ctx dta =
  Emit.set_type_map (fun id -> Context.get_binding id ctx);
  Emit.set_delta_map (fun id -> Context.get_alias_binding id dta);
  print_endline (Emit.generate_c ast)

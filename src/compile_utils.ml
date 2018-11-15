open Lexer
open Lexing

let show_ast print_ast msg ast =
  begin if print_ast then
    print_endline ("=======" ^ msg ^ "=========\n" ^ Ast.show_command ast)
  end;
  ast

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
    Type.typecheck ast
  with
    Error_msg.TypeError s -> failwith s

let emit_code ast ctx =
  Emit.set_type_map (fun id -> Context.get_binding id ctx);
  Emit.generate_c ast

let compile_string_without_print prog print_ast : string =
  Printexc.record_backtrace false;
  let ast_and_cap_ctx = parse_with_error prog
    |> show_ast print_ast "Parsed AST"
    |> Resolve_alias.remove_aliases
    |> show_ast print_ast "Resolved AST"
    |> Infer_cap.infer_cap
  in let ast = fst @@ ast_and_cap_ctx
    |> show_ast print_ast "Inferred capabilities"
    |> Flatten_seq.flatten_seq
    |> show_ast print_ast "Flattened"
  in let type_ctx = typecheck_with_error ast
  in let ast_caps = Infer_cap.readd_cap ast (snd ast_and_cap_ctx)
  in emit_code ast_caps type_ctx

let compile_string prog print_ast = print_endline @@ compile_string_without_print prog print_ast

open Seashell
open Lexer
open Lexing
open Context
open Cmdliner

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Core.fprintf outx "line %d, column %d"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Some (Parser.prog Lexer.token lexbuf) with
  | SyntaxError msg ->
    Core.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    Core.fprintf stderr "Syntax error on %a\n" print_position lexbuf;
    exit (-1)

let typecheck_with_error (ast : Ast.command) : (gamma * delta) option =
  try
    Some (Type.check_cmd ast (Context.empty_gamma, Context.empty_delta))
  with
    Type.TypeError s -> Core.fprintf stderr "%s\n" s; exit(-1)

let ( >>= ) opt f = match opt with
| Some v -> f v
| None -> None

let seac filename no_typecheck =
  let lexbuf = Lexing.from_string @@ Std.input_file filename in
  parse_with_error lexbuf  >>= fun ast -> begin
    if not no_typecheck then
      typecheck_with_error ast
    else
      Some (Context.empty_gamma, Context.empty_delta)
  end
  >>= fun (ctx, dta) ->
    Emit.set_type_map (fun id -> Context.get_binding id ctx);
    Emit.set_delta_map (fun id -> Context.get_alias_binding id dta);
    print_endline (Emit.generate_c ast); None

let filename =
  let doc = "The file to be compiler by the seashell compiler." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let no_typecheck =
  let doc = "Emit code without type checking." in
  Arg.(value & flag & info ["nt"; "no-typecheck"] ~doc)

let info =
  let doc = "The Seashell compiler" in
  Term.info "seac" ~exits:Term.default_exits ~doc

let seac_t = Term.(const seac $ filename $ no_typecheck)

let () = Term.exit @@ Term.eval (seac_t, info)

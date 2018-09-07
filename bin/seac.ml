open Seashell
open Lexer
open Lexing
open Context

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
    Type.TypeError s -> print_endline s; None

let no_typecheck = ref false

let usage = "usage: " ^ Sys.argv.(0) ^ "[-nt]"

let specs = [
  ("-nt", Arg.Set no_typecheck, ": turn off typechecking")
]

let ( >>= ) opt f = match opt with
| Some v -> f v
| None -> None

let _ =

  Arg.parse
    specs
    (fun s -> raise (Arg.Bad ("Invalid argument " ^ s)))
    usage;

  let lexbuf = Lexing.from_channel Core.In_channel.stdin in
  parse_with_error lexbuf  >>= fun ast ->
  typecheck_with_error ast >>= fun (ctx, dta) ->
    Emit.set_type_map (fun id -> Context.get_binding id ctx);
    Emit.set_delta_map (fun id -> Context.get_alias_binding id dta);
    print_endline (Emit.generate_c ast);
    None

open Seashell
open Lexer
open Lexing
open Context

let no_typecheck = ref false

let usage = "usage: " ^ Sys.argv.(0) ^ "[-nt]"

let specs = [
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
      let (ctx, dta) =
        Type.check_cmd commands (Context.empty_gamma, Context.empty_delta)
      in
      Emit.set_type_map (fun id -> Context.get_binding id ctx);
      Emit.set_delta_map (fun id -> Context.get_alias_binding id dta)
    else ();

    print_endline (Emit.generate_c commands)

  with
    Type.TypeError s -> print_endline s

open Seashell
open Lexer
open Lexing

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
      let (ctx, _) = 
        Type.check_cmd commands (Type.empty_context, Type.empty_delta) 
      in Emit.set_type_map (fun id -> Type.type_of_id id ctx)
    else ();
    
    print_endline (Emit.generate_c commands)
    
  with
    Type.TypeError s -> print_endline s

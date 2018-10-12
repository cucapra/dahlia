open Core
open Cmdliner
open Seashell
open Compile_utils

let seac filename print_ast : unit =
  Printexc.record_backtrace false;
  let prog = In_channel.read_all filename in
  compile_string prog print_ast

let filename =
  let doc = "The file to be compiler by the seashell compiler." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let print_ast =
  let doc = "Print the various intermediate states of the AST." in
  Arg.(value & flag & info ["p"; "print-ast"] ~doc)

let info =
  let doc = "The Seashell compiler" in
  Term.info "seac" ~exits:Term.default_exits ~doc

let seac_t = Term.(const seac $ filename $ print_ast)

let () = Term.exit @@ Term.eval (seac_t, info)

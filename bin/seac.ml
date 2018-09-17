open Core
open Cmdliner
open Seashell
open Compile_utils

let seac filename no_typecheck print_ast : unit =
  Printexc.record_backtrace false;
  let prog = In_channel.read_all filename in
  let ast = parse_with_error prog in
  begin if print_ast then
    print_endline ("=======Parsed AST=========\n" ^ Ast.show_command ast)
  end;
  let rast = Resolve_alias.remove_aliases ast in
  begin if print_ast then
    print_endline ("=======Resolved AST=========\n" ^ Ast.show_command rast)
  end;
  let ctx = begin
    if not no_typecheck then
      typecheck_with_error rast
    else
      Context.empty_gamma
  end in
  begin if print_ast then
    print_endline "==========================="
  end;
  emit_code rast ctx

let filename =
  let doc = "The file to be compiler by the seashell compiler." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILENAME" ~doc)

let no_typecheck =
  let doc = "Emit code without type checking." in
  Arg.(value & flag & info ["nt"; "no-typecheck"] ~doc)

let print_ast =
  let doc = "Print the various intermediate states of the AST." in
  Arg.(value & flag & info ["p"; "print-ast"] ~doc)

let info =
  let doc = "The Seashell compiler" in
  Term.info "seac" ~exits:Term.default_exits ~doc

let seac_t = Term.(const seac $ filename $ no_typecheck $ print_ast)

let () = Term.exit @@ Term.eval (seac_t, info)

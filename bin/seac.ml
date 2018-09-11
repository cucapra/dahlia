open Cmdliner
open Seashell
open Compile_utils

let seac filename no_typecheck =
  let prog = Std.input_file filename in
  let ast = parse_with_error prog in
  let (ctx, dta) = begin
    if not no_typecheck then
      typecheck_with_error ast
    else
      Context.empty_gamma, Context.empty_delta
  end in
  emit_code ast ctx dta

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

open Ast
open Ast_visitor

class flatten_seq_visitor = object
  inherit [unit] ast_mapper as super

  method! cseq cs st =
    let cs', _ = super#clist_visit cs st in
    let extract = function
      | CSeq cs -> cs
      | c -> [c]
    in CSeq (List.flatten @@ List.map extract cs'), ()
end

let flatten_seq cmd =
  fst @@ (new flatten_seq_visitor)#command cmd ()

open Ast
open Ast_visitor

module StringMap =
  Map.Make(struct type t = id;; let compare = String.compare end)

type delta = type_node StringMap.t

let add_alias_binding (id, t) d =
  StringMap.add id t d

let get_alias_binding id d =
  try StringMap.find id d
  with Not_found -> raise (Context.NoBinding id)

class type_alias_resolver = object
  inherit [delta] ast_mapper

  method! private talias id st = get_alias_binding id st, st

  method! private ctypedef bind st =
    CTypeDef (fst @@ bind, snd @@ bind), add_alias_binding bind st
end

let remove_aliases cmd =
  (new type_alias_resolver)#command cmd StringMap.empty |> fst

open Ast
open Error_msg

exception AlreadyConsumed
exception NoBinding of id

module StringMap =
  Map.Make(struct type t = id;; let compare = String.compare end)

type state =
  | Available
  | Consumed

type gamma = (state * type_node) StringMap.t

let empty_gamma = StringMap.empty

let add_binding id t g =
  if StringMap.mem id g then
    raise @@ TypeError (id_already_bound id)
  else
    StringMap.add id (Available, t) g

let get_binding id (g: gamma) =
  try snd @@ StringMap.find id g
  with Not_found -> raise (NoBinding id)

let consume_binding id g =
  if StringMap.mem id g then
    let (s, t) = StringMap.find id g in
    if s = Available then
      g |> StringMap.remove id |> StringMap.add id (Consumed, t)
    else
      raise AlreadyConsumed
  else
    raise (NoBinding id)

let renew_all_bindings g =
  StringMap.map (fun (_, t) -> (Available, t)) g

open Ast
open Error_msg

exception AlreadyConsumed of int
exception NoBinding of id

module StringMap =
  Map.Make(struct type t = id;; let compare = String.compare end)

module IntSet =
  Set.Make(struct
    type t = int
    let compare a b = if a > b then 1 else if a < b then -1 else 0
  end)

type gamma = {
  type_map : type_node StringMap.t ;
  indices_available: IntSet.t StringMap.t
}

let empty_gamma = {
  type_map = StringMap.empty ;
  indices_available = StringMap.empty
}

let create_set s = IntSet.of_list @@ Core.List.range 0 s;;

let compute_bf b =
  List.fold_left (fun acc (_, b) -> b * acc) 1 b

let add_binding id t g =
  if StringMap.mem id g.type_map then
    raise @@ TypeError (id_already_bound id)
  else
    let type_map' = StringMap.add id t g.type_map in
    let indices = match t with
    | TArray (_, banking) ->
        StringMap.add id (create_set (compute_bf banking)) g.indices_available
    | TLin _ ->
        StringMap.add id (create_set 1) g.indices_available
    | _ -> g.indices_available
    in
      {
        type_map = type_map' ;
        indices_available = indices
      }

let get_binding id g =
  try StringMap.find id g.type_map
  with Not_found -> raise (NoBinding id)

(** TODO(rachit): Don't give the same error message when accessing a consumed and
 * a non-existent memory bank. *)
let consume_aa id i g =
  if IntSet.mem i (StringMap.find id g.indices_available) then {
    g with indices_available =
      StringMap.add id
        (IntSet.remove i (StringMap.find id g.indices_available))
        g.indices_available
  }
  else raise (AlreadyConsumed i)

let consume_aa_lst id lst g =
  let consume_indices context bank =
    try consume_aa id bank context
    with AlreadyConsumed i -> raise (AlreadyConsumed i)
  in List.fold_left consume_indices g lst

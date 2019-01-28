open Ast

class ['s] ast_mapper = object(self)
  (** Some helper methods *)
  (** NOTE(rachit): Take that supporters of HM type inference!! *)
  method private list_visit : 'a. ('a -> 's -> 'a * 's) -> 'a list -> 's -> ('a list) * 's =
    fun f lst st -> match lst with
      | [] -> [], st
      | hd :: tl ->
          let hd', st1 = f hd st in
          let tl', st2 = self#list_visit f tl st1 in
          hd'::tl', st2

  method type_node (tn : type_node) (st : 's) = match tn with
    | TBool -> self#tbool st
    | TFloat -> self#tfloat st
    | TAlias t -> self#talias t st
    | TMux (id, i) -> self#tmux (id, i) st
    | TArray (t, is)  -> self#tarray (t, is) st
    | TIndex (s, d) -> self#tindex (s, d) st
    | TFunc ts -> self#tfunc ts st
    | TLin t -> self#tlin t st

  method private tbool st = TBool, st
  method private tfloat st = TFloat, st
  method private talias id st = TAlias id, st
  method private tmux (t1, t2) st = TMux (t1, t2), st
  method private tarray (t1, t2) st =
    let t1', st1 = self#type_node t1 st in TArray (t1', t2), st1
  method private tindex (t1, t2) st = TIndex (t1, t2), st
  method private tfunc ts st =
    let ts', st' = self#list_visit self#type_node ts st in TFunc ts', st'
  method private tlin t st =
    let t', st' = self#type_node t st in TLin t', st'

  method binop op st = op, st

  method expr (e : expr) (st : 's) : expr * 's = match e with
    | EInt i -> self#eint i st
    | EFloat f -> self#efloat f st
    | EVar v -> self#evar v st
    | EBool b -> self#ebool b st
    | EBinop (op, e1, e2) -> self#ebinop (op, e1, e2) st
    | EAA (id, es) -> self#eaa (id, es) st

  method private eint i st = EInt i, st
  method private ebool i st = EBool i, st
  method private evar i st = EVar i, st
  method private efloat i st = EFloat i, st
  method private ebinop (op, e1, e2) st =
    let op', st1 = self#binop op st in
    let e1', st2 = self#expr e1 st1 in
    let e2', st3 = self#expr e2 st2 in
    EBinop (op', e1', e2'), st3
  method private elist_visit lst st = self#list_visit self#expr lst st
  method private eaa (id, es) st =
    let es', st' = self#elist_visit es st in
    EAA (id, es'), st'

  method private capability cap st = cap, st

  method command (cmd : command) (st : 's) : command * 's = match cmd with
    | CCap (cap, e, id) -> self#ccap (cap, e, id) st
    | CAssign (id, e) -> self#cassign (id, e) st
    | CFor (id, e1, e2, i, c1, c2) -> self#cfor (id, e1, e2, i, c1, c2) st
    | CReassign (e1, e2) -> self#creassign (e1, e2) st
    | CIf (e, c) -> self#cif (e, c) st
    | CSeq cs -> self#cseq cs st
    | CFuncDef (i, g, c) -> self#cfuncdef (i, g, c) st
    | CTypeDef (i, t) -> self#ctypedef (i, t) st
    | CApp (id, es) -> self#capp (id, es) st
    | CExpr e -> self#cexpr e st
    | CEmpty -> self#cempty st

  method private clist_visit cs st = self#list_visit self#command cs st
  method private ccap (cap, e, id) st =
    let cap', st1 = self#capability cap st in
    let e', st2 = self#expr e st1 in
    CCap (cap', e', id), st2
  method private cassign (id, e) st =
    let e', st' = self#expr e st in
    CAssign (id, e'), st'
  method private cfor (id, e1, e2, i, c1, c2) st =
    let e1', st1 = self#expr e1 st in
    let e2', st2 = self#expr e2 st1 in
    let c1', st3 = self#command c1 st2 in
    let c2', st4 = self#command c2 st3 in
    CFor (id, e1', e2', i, c1', c2'), st4
  method private creassign (e1, e2) st =
    let e1', st1 = self#expr e1 st in
    let e2', st2 = self#expr e2 st1 in
    CReassign (e1', e2'), st2
  method private cif (e, c) st =
    let e', st1 = self#expr e st in
    let c', st2 = self#command c st1 in
    CIf (e', c'), st2
  method private cseq cs st =
    let cs', st' = self#clist_visit cs st in
    CSeq cs', st'
  method private cfuncdef (id, g, c) st =
    let f (i, t) st =
      let (t', s') = self#type_node t st in (i, t'), s' in
    let g', st1 = self#list_visit f g st in
    let c', st2 = self#command c st1 in
    CFuncDef (id, g', c'), st2
  method private ctypedef (id, t) st =
    let t', s' = self#type_node t st in
    CTypeDef (id, t'), s'
  method private capp (i, es) st =
    let es', st' = self#elist_visit es st in
    CApp (i, es'), st'
  method private cexpr e st =
    let e', s' = self#expr e st in
    CExpr e', s'
  method private cempty st = CEmpty, st
end

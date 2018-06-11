
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNROLL
    | TRUE
    | TIMES
    | SEMICOLON
    | RSQUARE
    | RPAREN
    | REASSIGN
    | RBRACK
    | RANGE_DOTS
    | PLUS
    | OR
    | NEQ
    | MINUS
    | LT
    | LSQUARE
    | LPAREN
    | LET
    | LEQ
    | LBRACK
    | INT_ANNOTATION
    | INT of (
# 8 "src/parser.mly"
       (int)
# 31 "src/parser.ml"
  )
    | IF
    | ID of (
# 9 "src/parser.mly"
       (string)
# 37 "src/parser.ml"
  )
    | GT
    | GEQ
    | FOR
    | FALSE
    | EQUAL
    | EQ
    | EOF
    | BOOL_ANNOTATION
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState74
  | MenhirState71
  | MenhirState70
  | MenhirState68
  | MenhirState60
  | MenhirState57
  | MenhirState55
  | MenhirState48
  | MenhirState45
  | MenhirState43
  | MenhirState40
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState11
  | MenhirState8
  | MenhirState5
  | MenhirState3
  | MenhirState0

# 1 "src/parser.mly"
  
open Ast
open Make_ast

# 96 "src/parser.ml"

let rec _menhir_run11 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 332 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RSQUARE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv157 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 362 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv155 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 369 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (x : (
# 9 "src/parser.mly"
       (string)
# 374 "src/parser.ml"
            ))), _, (index : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_expr = 
# 60 "src/parser.mly"
    ( make_array_access x index )
# 381 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)) : 'freshtv158)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv159 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 393 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)) : 'freshtv162)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv165 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv163 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _10 = () in
        let _v : 'tv_expr = let bop =
          let _1 = _10 in
          
# 85 "src/parser.mly"
          ( BopTimes )
# 409 "src/parser.ml"
          
        in
        
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 415 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv164)) : 'freshtv166)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv171 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv167 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let bop =
              let _1 = _10 in
              
# 83 "src/parser.mly"
         ( BopPlus )
# 436 "src/parser.ml"
              
            in
            
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 442 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv168)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv169 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)) : 'freshtv172)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv177 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv173 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let bop =
              let _1 = _10 in
              
# 87 "src/parser.mly"
       ( BopOr )
# 486 "src/parser.ml"
              
            in
            
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 492 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv175 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)) : 'freshtv178)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv183 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GT | LEQ | LT | NEQ | OR | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv179 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let bop =
              let _1 = _10 in
              
# 77 "src/parser.mly"
        ( BopNeq )
# 524 "src/parser.ml"
              
            in
            
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 530 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv180)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv181 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv189 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv185 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let bop =
              let _1 = _10 in
              
# 84 "src/parser.mly"
          ( BopMinus )
# 558 "src/parser.ml"
              
            in
            
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 564 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv186)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv187 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)) : 'freshtv190)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv195 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GT | LEQ | LT | NEQ | OR | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv191 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let bop =
              let _1 = _10 in
              
# 80 "src/parser.mly"
        ( BopLt )
# 596 "src/parser.ml"
              
            in
            
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 602 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv192)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv193 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)) : 'freshtv196)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv201 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GT | LEQ | LT | NEQ | OR | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv197 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let bop =
              let _1 = _10 in
              
# 79 "src/parser.mly"
        ( BopLeq )
# 634 "src/parser.ml"
              
            in
            
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 640 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv199 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv207 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GT | LEQ | LT | NEQ | OR | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv203 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let bop =
              let _1 = _10 in
              
# 81 "src/parser.mly"
        ( BopGt )
# 672 "src/parser.ml"
              
            in
            
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 678 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv205 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)) : 'freshtv208)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GT | LEQ | LT | NEQ | OR | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv209 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let bop =
              let _1 = _10 in
              
# 78 "src/parser.mly"
        ( BopGeq )
# 710 "src/parser.ml"
              
            in
            
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 716 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv211 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv219 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQ | GEQ | GT | LEQ | LT | NEQ | OR | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv215 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let bop =
              let _1 = _10 in
              
# 82 "src/parser.mly"
        ( BopEq )
# 748 "src/parser.ml"
              
            in
            
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 754 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv216)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv217 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)) : 'freshtv220)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv225 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_expr)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _10 = () in
            let _v : 'tv_expr = let bop =
              let _1 = _10 in
              
# 86 "src/parser.mly"
        ( BopAnd )
# 798 "src/parser.ml"
              
            in
            
# 70 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 804 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv222)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv233 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv229 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv227 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 58 "src/parser.mly"
    ( e )
# 852 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv228)) : 'freshtv230)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv231 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)) : 'freshtv234)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv241 * _menhir_state) * (
# 9 "src/parser.mly"
       (string)
# 869 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv237 * _menhir_state) * (
# 9 "src/parser.mly"
       (string)
# 899 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv235 * _menhir_state) * (
# 9 "src/parser.mly"
       (string)
# 906 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 9 "src/parser.mly"
       (string)
# 911 "src/parser.ml"
            ))), _, (e1 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_cmd = 
# 45 "src/parser.mly"
    ( make_assignment x e1 )
# 919 "src/parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)) : 'freshtv238)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv239 * _menhir_state) * (
# 9 "src/parser.mly"
       (string)
# 931 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv251 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv247 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACK ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv243 * _menhir_state)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | BOOL_ANNOTATION ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState43
                | FOR ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState43
                | ID _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
                | IF ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState43
                | INT_ANNOTATION ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43
                | LET ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv244)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv245 * _menhir_state)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv249 * _menhir_state)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv250)) : 'freshtv252)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv261 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1010 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RSQUARE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv257 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1040 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | REASSIGN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv253 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1050 "src/parser.ml"
                ))) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | FALSE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | ID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
                | INT _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
                | LPAREN ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv254)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv255 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1076 "src/parser.ml"
                ))) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)) : 'freshtv258)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv259 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1089 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)) : 'freshtv262)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv269 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1098 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv265 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1128 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv263 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1135 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (id : (
# 9 "src/parser.mly"
       (string)
# 1140 "src/parser.ml"
            ))), _, (index : 'tv_expr)), _, (e : 'tv_expr)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_cmd = 
# 49 "src/parser.mly"
    ( make_array_update id index e )
# 1149 "src/parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)) : 'freshtv266)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv267 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1161 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)) : 'freshtv270)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv275 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1170 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RANGE_DOTS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv271 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1200 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | INT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv272)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv273 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1228 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv285 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1237 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv281 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1267 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACK ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((((('freshtv277 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1277 "src/parser.ml"
                ))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | BOOL_ANNOTATION ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | FOR ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | ID _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
                | IF ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | INT_ANNOTATION ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | LET ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv278)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((((('freshtv279 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1305 "src/parser.ml"
                ))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)) : 'freshtv282)
        | TIMES ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv283 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1318 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)) : 'freshtv286)
    | _ ->
        _menhir_fail ()

and _menhir_goto_cmd : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_cmd -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv131 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1334 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL_ANNOTATION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | FOR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | ID _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | IF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | INT_ANNOTATION ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LET ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv129 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1356 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState68 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv127 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1364 "src/parser.ml"
            ))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((((_menhir_stack, _menhir_s), (x : (
# 9 "src/parser.mly"
       (string)
# 1370 "src/parser.ml"
            ))), _, (x1 : 'tv_expr)), _, (x2 : 'tv_expr)), _, (c : 'tv_cmd)) = _menhir_stack in
            let _12 = () in
            let _10 = () in
            let _9 = () in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_cmd = 
# 54 "src/parser.mly"
    ( make_for x x1 x2 c )
# 1383 "src/parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv128)) : 'freshtv130)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv132)
    | MenhirState74 | MenhirState71 | MenhirState70 | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * _menhir_state * 'tv_cmd) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL_ANNOTATION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FOR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | ID _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | IF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | INT_ANNOTATION ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LET ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | EOF | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv133 * _menhir_state * 'tv_cmd) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (c1 : 'tv_cmd)), _, (c2 : 'tv_cmd)) = _menhir_stack in
            let _v : 'tv_cmd = 
# 43 "src/parser.mly"
    ( make_seq c1 c2 )
# 1415 "src/parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv134)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv136)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv141 * _menhir_state)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL_ANNOTATION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | FOR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | ID _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | IF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | INT_ANNOTATION ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LET ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv139 * _menhir_state)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState71 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv137 * _menhir_state)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s), _, (b : 'tv_expr)), _, (body : 'tv_cmd)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_cmd = 
# 47 "src/parser.mly"
    ( make_if b body )
# 1457 "src/parser.ml"
             in
            _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv138)) : 'freshtv140)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv142)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL_ANNOTATION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv151 * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState74 in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (c : 'tv_cmd)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 33 "src/parser.mly"
       (Ast.command)
# 1484 "src/parser.ml"
            ) = 
# 39 "src/parser.mly"
    ( c )
# 1488 "src/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv147) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 33 "src/parser.mly"
       (Ast.command)
# 1496 "src/parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 33 "src/parser.mly"
       (Ast.command)
# 1504 "src/parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 33 "src/parser.mly"
       (Ast.command)
# 1512 "src/parser.ml"
            )) : (
# 33 "src/parser.mly"
       (Ast.command)
# 1516 "src/parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv144)) : 'freshtv146)) : 'freshtv148)) : 'freshtv150)) : 'freshtv152)
        | FOR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ID _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | IF ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | INT_ANNOTATION ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LET ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv154)
    | _ ->
        _menhir_fail ()

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 64 "src/parser.mly"
    ( make_bool true )
# 1546 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv126)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "src/parser.mly"
       (int)
# 1574 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : (
# 8 "src/parser.mly"
       (int)
# 1584 "src/parser.ml"
    )) : (
# 8 "src/parser.mly"
       (int)
# 1588 "src/parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 62 "src/parser.mly"
    ( make_int x )
# 1593 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "src/parser.mly"
       (string)
# 1600 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LSQUARE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1612 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | ID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | INT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv118)
    | AND | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS | RANGE_DOTS | RPAREN | RSQUARE | SEMICOLON | TIMES ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1636 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (x : (
# 9 "src/parser.mly"
       (string)
# 1641 "src/parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_expr = 
# 68 "src/parser.mly"
    ( make_var x )
# 1646 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv120)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1656 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 66 "src/parser.mly"
    ( make_bool false )
# 1671 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv116)

and _menhir_goto_type_annotation : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_type_annotation -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv113 * _menhir_state * 'tv_type_annotation) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state * 'tv_type_annotation) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "src/parser.mly"
       (string)
# 1689 "src/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LSQUARE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv105 * _menhir_state * 'tv_type_annotation) * (
# 9 "src/parser.mly"
       (string)
# 1700 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv101 * _menhir_state * 'tv_type_annotation) * (
# 9 "src/parser.mly"
       (string)
# 1710 "src/parser.ml"
                ))) = Obj.magic _menhir_stack in
                let (_v : (
# 8 "src/parser.mly"
       (int)
# 1715 "src/parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | RSQUARE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv97 * _menhir_state * 'tv_type_annotation) * (
# 9 "src/parser.mly"
       (string)
# 1726 "src/parser.ml"
                    ))) * (
# 8 "src/parser.mly"
       (int)
# 1730 "src/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | SEMICOLON ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv93 * _menhir_state * 'tv_type_annotation) * (
# 9 "src/parser.mly"
       (string)
# 1740 "src/parser.ml"
                        ))) * (
# 8 "src/parser.mly"
       (int)
# 1744 "src/parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv91 * _menhir_state * 'tv_type_annotation) * (
# 9 "src/parser.mly"
       (string)
# 1751 "src/parser.ml"
                        ))) * (
# 8 "src/parser.mly"
       (int)
# 1755 "src/parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _menhir_s, (t : 'tv_type_annotation)), (x : (
# 9 "src/parser.mly"
       (string)
# 1760 "src/parser.ml"
                        ))), (s : (
# 8 "src/parser.mly"
       (int)
# 1764 "src/parser.ml"
                        ))) = _menhir_stack in
                        let _6 = () in
                        let _5 = () in
                        let _3 = () in
                        let _v : 'tv_cmd = 
# 51 "src/parser.mly"
    ( make_assignment x (make_array s t) )
# 1772 "src/parser.ml"
                         in
                        _menhir_goto_cmd _menhir_env _menhir_stack _menhir_s _v) : 'freshtv92)) : 'freshtv94)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv95 * _menhir_state * 'tv_type_annotation) * (
# 9 "src/parser.mly"
       (string)
# 1782 "src/parser.ml"
                        ))) * (
# 8 "src/parser.mly"
       (int)
# 1786 "src/parser.ml"
                        ))) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)) : 'freshtv98)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv99 * _menhir_state * 'tv_type_annotation) * (
# 9 "src/parser.mly"
       (string)
# 1797 "src/parser.ml"
                    ))) * (
# 8 "src/parser.mly"
       (int)
# 1801 "src/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)) : 'freshtv102)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv103 * _menhir_state * 'tv_type_annotation) * (
# 9 "src/parser.mly"
       (string)
# 1812 "src/parser.ml"
                ))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)) : 'freshtv106)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv107 * _menhir_state * 'tv_type_annotation) * (
# 9 "src/parser.mly"
       (string)
# 1823 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)) : 'freshtv110)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state * 'tv_type_annotation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)) : 'freshtv114)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv41 * _menhir_state)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_cmd) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv45 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1858 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr))) * _menhir_state * 'tv_cmd) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv47 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1867 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv49 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1876 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv51 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 1885 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv53 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1894 "src/parser.ml"
        ))) * _menhir_state * 'tv_expr))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1903 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv57 * _menhir_state)) * _menhir_state * 'tv_expr))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 1977 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state) * (
# 9 "src/parser.mly"
       (string)
# 1991 "src/parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv90)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "src/parser.mly"
       (string)
# 2012 "src/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv31 * _menhir_state) * (
# 9 "src/parser.mly"
       (string)
# 2023 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | INT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv32)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv33 * _menhir_state) * (
# 9 "src/parser.mly"
       (string)
# 2049 "src/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)) : 'freshtv36)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_type_annotation = 
# 74 "src/parser.mly"
                   ( TInt )
# 2071 "src/parser.ml"
     in
    _menhir_goto_type_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv30)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | ID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | INT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv26)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "src/parser.mly"
       (string)
# 2112 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LSQUARE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 2124 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | ID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | INT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * (
# 9 "src/parser.mly"
       (string)
# 2150 "src/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv9 * _menhir_state))) = Obj.magic _menhir_stack in
                let (_v : (
# 9 "src/parser.mly"
       (string)
# 2179 "src/parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | EQUAL ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv5 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 2190 "src/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | FALSE ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | ID _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
                    | INT _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
                    | LPAREN ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | TRUE ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv6)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv7 * _menhir_state))) * (
# 9 "src/parser.mly"
       (string)
# 2216 "src/parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)) : 'freshtv10)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv11 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)) : 'freshtv14)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv15 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)) : 'freshtv18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_type_annotation = 
# 73 "src/parser.mly"
                    ( TBool )
# 2252 "src/parser.ml"
     in
    _menhir_goto_type_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 33 "src/parser.mly"
       (Ast.command)
# 2271 "src/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_ANNOTATION ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FOR ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT_ANNOTATION ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 219 "/Users/ted/.opam/4.06.0/lib/menhir/standard.mly"
  


# 2311 "src/parser.ml"

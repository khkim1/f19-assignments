
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITHOUT
    | VAR of (
# 5 "src/grammar.mly"
       (string)
# 12 "src/grammar.ml"
  )
    | UNFOLD
    | TY_UNIT
    | TY_NUM
    | TY_BOOL
    | TRUE
    | THEN
    | TFN
    | SUB
    | RPAREN
    | RIGHT
    | REC
    | RBRACKET
    | RBRACE
    | PLUS
    | OR
    | NUM of (
# 53 "src/grammar.mly"
       (int)
# 32 "src/grammar.ml"
  )
    | MUL
    | LT
    | LPAREN
    | LETREC
    | LET
    | LEFT
    | LBRACKET
    | LBRACE
    | INJECT
    | IN
    | IMPORT
    | IF
    | GT
    | FORALL
    | FOLD
    | FN
    | FALSE
    | EXPORT
    | EXISTS
    | EQ
    | EOF
    | ELSE
    | DOT
    | DIV
    | COMMA
    | COLON
    | CASE
    | BAR
    | AS
    | ARROW
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
  | MenhirState148
  | MenhirState146
  | MenhirState141
  | MenhirState138
  | MenhirState135
  | MenhirState132
  | MenhirState129
  | MenhirState127
  | MenhirState124
  | MenhirState121
  | MenhirState119
  | MenhirState116
  | MenhirState113
  | MenhirState110
  | MenhirState108
  | MenhirState105
  | MenhirState102
  | MenhirState95
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState74
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState64
  | MenhirState62
  | MenhirState59
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState46
  | MenhirState45
  | MenhirState43
  | MenhirState40
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState23
  | MenhirState20
  | MenhirState19
  | MenhirState12
  | MenhirState8
  | MenhirState6
  | MenhirState2
  | MenhirState0

# 1 "src/grammar.mly"
  
  open Ast

# 137 "src/grammar.ml"

let rec _menhir_goto_dir : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.Expr.direction) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e : (Ast.Expr.t))), _, (d : (Ast.Expr.direction))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.Expr.t) = 
# 94 "src/grammar.mly"
                        ( Expr.Project { e ; d } )
# 151 "src/grammar.ml"
         in
        _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXISTS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | FORALL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | REC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | TY_BOOL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | TY_NUM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | TY_UNIT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | VAR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.Expr.direction) = 
# 145 "src/grammar.mly"
        (Expr.Right)
# 201 "src/grammar.ml"
     in
    _menhir_goto_dir _menhir_env _menhir_stack _menhir_s _v

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.Expr.direction) = 
# 144 "src/grammar.mly"
       (Expr.Left)
# 213 "src/grammar.ml"
     in
    _menhir_goto_dir _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CASE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EXPORT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | FALSE ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | FN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | FOLD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | IF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | IMPORT ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | INJECT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LETREC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NUM _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | TFN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | UNFOLD ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | AND | AS | BAR | COMMA | DIV | DOT | ELSE | EOF | EQ | GT | IN | LBRACE | LBRACKET | LT | MUL | OR | PLUS | RBRACE | RPAREN | SUB | THEN | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (x : (
# 5 "src/grammar.mly"
       (string)
# 263 "src/grammar.ml"
            ))), _, (tau : (Ast.Type.t))), _, (e : (Ast.Expr.t))) = _menhir_stack in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.Expr.t) = 
# 88 "src/grammar.mly"
  ( Expr.Lam { x ; tau ; e } )
# 273 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CASE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | EXPORT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | FALSE ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | FN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | FOLD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | IF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | IMPORT ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | INJECT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LETREC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | NUM _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState141 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.Expr.t))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.Expr.t) = 
# 114 "src/grammar.mly"
                         ( e )
# 320 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | TFN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | UNFOLD ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CASE ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState146 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.Expr.t))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 65 "src/grammar.mly"
       (Ast.Expr.t)
# 351 "src/grammar.ml"
            ) = 
# 71 "src/grammar.mly"
               ( e )
# 355 "src/grammar.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 65 "src/grammar.mly"
       (Ast.Expr.t)
# 362 "src/grammar.ml"
            )) = _v in
            Obj.magic _1
        | EXPORT ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | FALSE ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | FN ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | FOLD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | IF ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | IMPORT ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | INJECT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | LET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | LETREC ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | NUM _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | TFN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | TRUE ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | UNFOLD ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
    | _ ->
        _menhir_fail ()

and _menhir_reduce5 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (e : (Ast.Expr.t))) = _menhir_stack in
    let _v : (Ast.Expr.t) = 
# 78 "src/grammar.mly"
            ( e )
# 408 "src/grammar.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXISTS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | FORALL ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | REC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TY_BOOL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TY_NUM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TY_UNIT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | VAR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | RIGHT ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.Type.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), (a : (
# 5 "src/grammar.mly"
       (string)
# 845 "src/grammar.ml"
        ))), _, (tau : (Ast.Type.t))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.Type.t) = 
# 130 "src/grammar.mly"
                              ( Type.Exists { a; tau } )
# 852 "src/grammar.ml"
         in
        _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), (a : (
# 5 "src/grammar.mly"
       (string)
# 861 "src/grammar.ml"
        ))), _, (tau : (Ast.Type.t))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.Type.t) = 
# 129 "src/grammar.mly"
                              ( Type.Forall { a; tau } )
# 868 "src/grammar.ml"
         in
        _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Ast.Type.t))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.Type.t) = 
# 141 "src/grammar.mly"
                       ( t )
# 886 "src/grammar.ml"
             in
            _menhir_goto_ty2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), (a : (
# 5 "src/grammar.mly"
       (string)
# 901 "src/grammar.ml"
        ))), _, (tau : (Ast.Type.t))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.Type.t) = 
# 128 "src/grammar.mly"
                           ( Type.Rec { a; tau } )
# 908 "src/grammar.ml"
         in
        _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | EXPORT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | FALSE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | FN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | FOLD ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | IF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | IMPORT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | INJECT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | LET ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | LETREC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | TFN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | UNFOLD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | EXPORT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | FALSE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | FN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | FOLD ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | IF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | IMPORT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | INJECT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LET ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LETREC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | TFN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | UNFOLD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CASE ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | EXPORT ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | FALSE ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | FN ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | FOLD ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | IF ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | IMPORT ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | INJECT ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | LET ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | LETREC ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | LPAREN ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | NUM _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                | TFN ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | TRUE ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | UNFOLD ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | VAR _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Ast.Expr.t))), _, (tau : (Ast.Type.t))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.Expr.t) = 
# 106 "src/grammar.mly"
                                       ( Expr.TyApp { e ; tau } )
# 1093 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXISTS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | FORALL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | REC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | TY_BOOL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | TY_NUM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | TY_UNIT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | VAR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (e : (Ast.Expr.t))), _, (tau_adt : (Ast.Type.t))), _, (tau_mod : (Ast.Type.t))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.Expr.t) = 
# 110 "src/grammar.mly"
  ( Expr.Export { e; tau_adt; tau_mod } )
# 1148 "src/grammar.ml"
         in
        _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (e : (Ast.Expr.t))), _, (tau : (Ast.Type.t))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.Expr.t) = 
# 107 "src/grammar.mly"
                             ( Expr.Fold_ { e; tau } )
# 1160 "src/grammar.ml"
         in
        _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (e : (Ast.Expr.t))), _, (d : (Ast.Expr.direction))), _, (tau : (Ast.Type.t))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.Expr.t) = 
# 95 "src/grammar.mly"
                                          ( Expr.Inject { e ; d ; tau } )
# 1173 "src/grammar.ml"
         in
        _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (Ast.Type.t))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 66 "src/grammar.mly"
       (Ast.Type.t)
# 1189 "src/grammar.ml"
            ) = 
# 74 "src/grammar.mly"
             ( t )
# 1193 "src/grammar.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 66 "src/grammar.mly"
       (Ast.Type.t)
# 1200 "src/grammar.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Type.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TY_BOOL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TY_NUM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TY_UNIT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | VAR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Type.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TY_BOOL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TY_NUM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TY_UNIT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | VAR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.Type.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TY_BOOL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TY_NUM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TY_UNIT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | VAR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_goto_expr2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | VAR _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | RPAREN ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | ARROW ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | CASE ->
                                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | EXPORT ->
                                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | FALSE ->
                                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | FN ->
                                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | FOLD ->
                                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | IF ->
                                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | IMPORT ->
                                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | INJECT ->
                                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | LET ->
                                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | LETREC ->
                                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | LPAREN ->
                                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | NUM _v ->
                                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                                | TFN ->
                                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | TRUE ->
                                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | UNFOLD ->
                                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                                | VAR _v ->
                                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | LT | NUM _ | OR | PLUS | RBRACE | RPAREN | SUB | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Expr.t))), _, (right : (Ast.Expr.t))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.Expr.t) = let binop = 
# 118 "src/grammar.mly"
      ( Expr.Sub )
# 1426 "src/grammar.ml"
             in
            
# 82 "src/grammar.mly"
  ( Expr.Binop { binop ; left ; right } )
# 1431 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | DIV | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | LT | MUL | NUM _ | OR | PLUS | RBRACE | RPAREN | SUB | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Expr.t))), _, (right : (Ast.Expr.t))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.Expr.t) = let binop = 
# 119 "src/grammar.mly"
      ( Expr.Mul )
# 1456 "src/grammar.ml"
             in
            
# 82 "src/grammar.mly"
  ( Expr.Binop { binop ; left ; right } )
# 1461 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | DIV | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | LT | MUL | NUM _ | OR | PLUS | RBRACE | RPAREN | SUB | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Expr.t))), _, (right : (Ast.Expr.t))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.Expr.t) = let binop = 
# 120 "src/grammar.mly"
      ( Expr.Div )
# 1486 "src/grammar.ml"
             in
            
# 82 "src/grammar.mly"
  ( Expr.Binop { binop ; left ; right } )
# 1491 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | LT | NUM _ | OR | PLUS | RBRACE | RPAREN | SUB | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Expr.t))), _, (right : (Ast.Expr.t))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.Expr.t) = let binop = 
# 117 "src/grammar.mly"
       ( Expr.Add )
# 1520 "src/grammar.ml"
             in
            
# 82 "src/grammar.mly"
  ( Expr.Binop { binop ; left ; right } )
# 1525 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | ELSE | EOF | EXPORT | FALSE | FN | FOLD | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | NUM _ | OR | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Expr.t))), _, (right : (Ast.Expr.t))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.Expr.t) = 
# 86 "src/grammar.mly"
                                ( Expr.Or { left ; right } )
# 1564 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | LT | NUM _ | OR | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Expr.t))), _, (right : (Ast.Expr.t))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.Expr.t) = let relop = 
# 125 "src/grammar.mly"
     ( Expr.Lt )
# 1597 "src/grammar.ml"
             in
            
# 84 "src/grammar.mly"
  ( Expr.Relop { relop ; left ; right }  )
# 1602 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | LT | NUM _ | OR | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Expr.t))), _, (right : (Ast.Expr.t))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.Expr.t) = let relop = 
# 124 "src/grammar.mly"
     ( Expr.Gt )
# 1635 "src/grammar.ml"
             in
            
# 84 "src/grammar.mly"
  ( Expr.Relop { relop ; left ; right }  )
# 1640 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState127 | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | LT | NUM _ | OR | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Expr.t))), _, (right : (Ast.Expr.t))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.Expr.t) = let relop = 
# 123 "src/grammar.mly"
     ( Expr.Eq )
# 1673 "src/grammar.ml"
             in
            
# 84 "src/grammar.mly"
  ( Expr.Relop { relop ; left ; right }  )
# 1678 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RIGHT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | VAR _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | RPAREN ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | ARROW ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | CASE ->
                                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | EXPORT ->
                                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | FALSE ->
                                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | FN ->
                                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | FOLD ->
                                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | IF ->
                                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | IMPORT ->
                                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | INJECT ->
                                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | LET ->
                                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | LETREC ->
                                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | LPAREN ->
                                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | NUM _v ->
                                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
                                | TFN ->
                                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | TRUE ->
                                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | UNFOLD ->
                                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                                | VAR _v ->
                                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e : (Ast.Expr.t))), (xleft : (
# 5 "src/grammar.mly"
       (string)
# 1849 "src/grammar.ml"
            ))), _, (eleft : (Ast.Expr.t))), (xright : (
# 5 "src/grammar.mly"
       (string)
# 1853 "src/grammar.ml"
            ))), _, (eright : (Ast.Expr.t))) = _menhir_stack in
            let _17 = () in
            let _15 = () in
            let _14 = () in
            let _12 = () in
            let _11 = () in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.Expr.t) = 
# 97 "src/grammar.mly"
  ( Expr.Case { e ; xleft; eleft; xright; eright } )
# 1870 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | ELSE | EOF | EXPORT | FALSE | FN | FOLD | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | NUM _ | OR | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Expr.t))), _, (right : (Ast.Expr.t))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.Expr.t) = 
# 85 "src/grammar.mly"
                                 ( Expr.And { left ; right } )
# 1911 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXISTS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | FORALL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | REC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | TY_BOOL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | TY_NUM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | TY_UNIT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | VAR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AS | BAR | CASE | COMMA | ELSE | EOF | EXPORT | FALSE | FN | FOLD | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | NUM _ | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState146 | MenhirState141 | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AS | BAR | CASE | COMMA | ELSE | EOF | EXPORT | FALSE | FN | FOLD | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | NUM _ | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lam : (Ast.Expr.t))), _, (arg : (Ast.Expr.t))) = _menhir_stack in
            let _v : (Ast.Expr.t) = 
# 77 "src/grammar.mly"
                         ( Expr.App { lam ; arg } )
# 2046 "src/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | AS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXISTS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | FORALL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | REC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | TY_BOOL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | TY_NUM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | TY_UNIT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | VAR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | EXPORT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | FALSE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | FN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | FOLD ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | IF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | IMPORT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | INJECT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | LET ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | LETREC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | TFN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | UNFOLD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | EXPORT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | FALSE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | FN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | FOLD ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | IF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | IMPORT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | INJECT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LET ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LETREC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | TFN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | UNFOLD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AS | BAR | CASE | COMMA | ELSE | EOF | EXPORT | FALSE | FN | FOLD | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | NUM _ | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (cond : (Ast.Expr.t))), _, (then_ : (Ast.Expr.t))), _, (else_ : (Ast.Expr.t))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.Expr.t) = 
# 99 "src/grammar.mly"
  ( Expr.If { cond; then_; else_ } )
# 2297 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | EXPORT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | FALSE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | FN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | FOLD ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | IF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | IMPORT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | INJECT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | LET ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | LETREC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | TFN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | UNFOLD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AS | BAR | CASE | COMMA | ELSE | EOF | EXPORT | FALSE | FN | FOLD | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | NUM _ | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (
# 5 "src/grammar.mly"
       (string)
# 2412 "src/grammar.ml"
            ))), (a : (
# 5 "src/grammar.mly"
       (string)
# 2416 "src/grammar.ml"
            ))), _, (e_mod : (Ast.Expr.t))), _, (e_body : (Ast.Expr.t))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.Expr.t) = 
# 112 "src/grammar.mly"
  ( Expr.Import { x; a; e_mod; e_body } )
# 2427 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | EXPORT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | FALSE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | FN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | FOLD ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | IF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | IMPORT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | INJECT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LEFT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LET ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LETREC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | RIGHT ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | TFN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | UNFOLD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | EXPORT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FALSE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FOLD ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | IF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | IMPORT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | INJECT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LET ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LETREC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | TFN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | UNFOLD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AS | BAR | CASE | COMMA | ELSE | EOF | EXPORT | FALSE | FN | FOLD | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | NUM _ | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (
# 5 "src/grammar.mly"
       (string)
# 2618 "src/grammar.ml"
            ))), _, (tau : (Ast.Type.t))), _, (evar : (Ast.Expr.t))), _, (ebody : (Ast.Expr.t))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.Expr.t) = 
# 101 "src/grammar.mly"
  ( Expr.App { lam = Expr.Lam { x ; tau ; e = ebody } ; arg = evar } )
# 2627 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | EXPORT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | FALSE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | FN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | FOLD ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | IF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | IMPORT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | INJECT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | LET ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | LETREC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | TFN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | UNFOLD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AS | BAR | CASE | COMMA | ELSE | EOF | EXPORT | FALSE | FN | FOLD | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | NUM _ | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (
# 5 "src/grammar.mly"
       (string)
# 2742 "src/grammar.ml"
            ))), _, (tau : (Ast.Type.t))), _, (evar : (Ast.Expr.t))), _, (ebody : (Ast.Expr.t))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.Expr.t) = 
# 103 "src/grammar.mly"
  ( Expr.App { lam = Expr.Lam {x ; tau ; e = ebody} ;
               arg = Expr.Fix { x ; tau ; e = evar } } )
# 2752 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | EXPORT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | FALSE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | FN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | FOLD ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | IF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | IMPORT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | INJECT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LET ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LETREC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | TFN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | UNFOLD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | CASE | EXPORT | FALSE | FN | FOLD | IF | IMPORT | INJECT | LET | LETREC | LPAREN | NUM _ | RPAREN | TFN | TRUE | UNFOLD | VAR _ ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (left : (Ast.Expr.t))), _, (right : (Ast.Expr.t))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.Expr.t) = 
# 93 "src/grammar.mly"
                                                 ( Expr.Pair { left ; right } )
# 2873 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AS | BAR | CASE | COMMA | ELSE | EOF | EXPORT | FALSE | FN | FOLD | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | NUM _ | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (a : (
# 5 "src/grammar.mly"
       (string)
# 2916 "src/grammar.ml"
            ))), _, (e : (Ast.Expr.t))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.Expr.t) = 
# 105 "src/grammar.mly"
                              ( Expr.TyLam { a ; e } )
# 2923 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | AS | BAR | CASE | COMMA | ELSE | EOF | EXPORT | FALSE | FN | FOLD | IF | IMPORT | IN | INJECT | LBRACE | LET | LETREC | LPAREN | NUM _ | RBRACE | RPAREN | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.Expr.t))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.Expr.t) = 
# 108 "src/grammar.mly"
                   ( Expr.Unfold e )
# 2966 "src/grammar.ml"
             in
            _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ty2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.Type.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState148 | MenhirState129 | MenhirState116 | MenhirState110 | MenhirState108 | MenhirState71 | MenhirState59 | MenhirState43 | MenhirState12 | MenhirState19 | MenhirState20 | MenhirState23 | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | DIV | DOT | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LBRACKET | LET | LETREC | LPAREN | LT | NUM _ | OR | RBRACE | RBRACKET | RPAREN | SUB | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (Ast.Type.t))) = _menhir_stack in
            let _v : (Ast.Type.t) = 
# 131 "src/grammar.mly"
          ( t )
# 2999 "src/grammar.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | DIV | DOT | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LBRACKET | LET | LETREC | LPAREN | LT | NUM _ | OR | RBRACE | RBRACKET | RPAREN | SUB | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Type.t))), _, (right : (Ast.Type.t))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.Type.t) = 
# 138 "src/grammar.mly"
                              ( Type.Sum { left; right } )
# 3026 "src/grammar.ml"
             in
            _menhir_goto_ty2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | DIV | DOT | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LBRACKET | LET | LETREC | LPAREN | LT | NUM _ | OR | RBRACE | RBRACKET | RPAREN | SUB | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Ast.Type.t))), _, (right : (Ast.Type.t))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.Type.t) = 
# 139 "src/grammar.mly"
                             ( Type.Product { left; right } )
# 3053 "src/grammar.ml"
             in
            _menhir_goto_ty2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | AND | AS | BAR | CASE | COMMA | DIV | DOT | ELSE | EOF | EQ | EXPORT | FALSE | FN | FOLD | GT | IF | IMPORT | IN | INJECT | LBRACE | LBRACKET | LET | LETREC | LPAREN | LT | NUM _ | OR | RBRACE | RBRACKET | RPAREN | SUB | TFN | THEN | TRUE | UNFOLD | VAR _ | WITHOUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (arg : (Ast.Type.t))), _, (ret : (Ast.Type.t))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.Type.t) = 
# 140 "src/grammar.mly"
                            ( Type.Fn { arg; ret } )
# 3080 "src/grammar.ml"
             in
            _menhir_goto_ty2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "src/grammar.mly"
       (string)
# 3095 "src/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (v : (
# 5 "src/grammar.mly"
       (string)
# 3103 "src/grammar.ml"
    )) = _v in
    let _v : (Ast.Expr.t) = 
# 90 "src/grammar.mly"
          ( Expr.Var(v) )
# 3108 "src/grammar.ml"
     in
    _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.Expr.t) = 
# 91 "src/grammar.mly"
       ( Expr.True )
# 3163 "src/grammar.ml"
     in
    _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | EXPORT ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | FALSE ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | FN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | FOLD ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | IF ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | IMPORT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | INJECT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LET ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LETREC ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | NUM _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | TFN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | UNFOLD ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 53 "src/grammar.mly"
       (int)
# 3236 "src/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 53 "src/grammar.mly"
       (int)
# 3244 "src/grammar.ml"
    )) = _v in
    let _v : (Ast.Expr.t) = 
# 89 "src/grammar.mly"
          ( Expr.Num(n) )
# 3249 "src/grammar.ml"
     in
    _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState8 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.Expr.t) = 
# 113 "src/grammar.mly"
                ( Expr.Unit )
# 3294 "src/grammar.ml"
         in
        _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXISTS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | FORALL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | REC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | TY_BOOL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | TY_NUM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | TY_UNIT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | VAR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXISTS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | FORALL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | REC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | TY_BOOL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | TY_NUM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | TY_UNIT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | VAR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | VAR _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | RPAREN ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | EQ ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | CASE ->
                                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | EXPORT ->
                                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | FALSE ->
                                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | FN ->
                                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | FOLD ->
                                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | IF ->
                                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | IMPORT ->
                                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | INJECT ->
                                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | LET ->
                                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | LETREC ->
                                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | LPAREN ->
                                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | NUM _v ->
                                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
                            | TFN ->
                                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | TRUE ->
                                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | UNFOLD ->
                                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                            | VAR _v ->
                                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | VAR _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EXISTS ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | FORALL ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | LPAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | REC ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | TY_BOOL ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | TY_NUM ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | TY_UNIT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | VAR _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.Expr.t) = 
# 92 "src/grammar.mly"
        ( Expr.False )
# 3719 "src/grammar.ml"
     in
    _menhir_goto_expr2 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "src/grammar.mly"
       (string)
# 4018 "src/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (a : (
# 5 "src/grammar.mly"
       (string)
# 4026 "src/grammar.ml"
    )) = _v in
    let _v : (Ast.Type.t) = 
# 137 "src/grammar.mly"
          ( Type.Var a )
# 4031 "src/grammar.ml"
     in
    _menhir_goto_ty2 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.Type.t) = 
# 136 "src/grammar.mly"
          ( Type.Unit )
# 4043 "src/grammar.ml"
     in
    _menhir_goto_ty2 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.Type.t) = 
# 134 "src/grammar.mly"
         ( Type.Num )
# 4055 "src/grammar.ml"
     in
    _menhir_goto_ty2 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.Type.t) = 
# 135 "src/grammar.mly"
          ( Type.Bool )
# 4067 "src/grammar.ml"
     in
    _menhir_goto_ty2 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXISTS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | FORALL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | REC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | TY_BOOL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | TY_NUM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | TY_UNIT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | VAR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXISTS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | FORALL ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | REC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TY_BOOL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TY_NUM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | TY_UNIT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | VAR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXISTS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | FORALL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | REC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | TY_BOOL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | TY_NUM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | TY_UNIT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | VAR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXISTS ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | FORALL ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | REC ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | TY_BOOL ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | TY_NUM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | TY_UNIT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | VAR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and expr_toplevel : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 65 "src/grammar.mly"
       (Ast.Expr.t)
# 4273 "src/grammar.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EXPORT ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FOLD ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IMPORT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INJECT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LETREC ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NUM _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TFN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UNFOLD ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and type_toplevel : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 66 "src/grammar.mly"
       (Ast.Type.t)
# 4321 "src/grammar.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXISTS ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | FORALL ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | REC ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | TY_BOOL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | TY_NUM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | TY_UNIT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | VAR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)

# 269 "<standard.mly>"
  

# 4353 "src/grammar.ml"


exception Error

let _eRR =
  Error

type token = 
  | WHERE
  | VALUES
  | UPDATE
  | UNION
  | TSTRING
  | TRUE
  | TOP
  | TINT
  | TFLOAT
  | TBOOL
  | TABLE
  | STRING of (string)
  | SET
  | SEMICOLON
  | SEL
  | SCATT
  | RPAREN
  | PLOT
  | PERCENT
  | ORDER
  | OR
  | ON
  | NE
  | LT
  | LPAREN
  | LINE
  | LE
  | JOIN
  | INTO
  | INT of (int)
  | INSERT
  | ID of (string)
  | HISTOG
  | GT
  | GE
  | FROM
  | FLOAT of (float)
  | FALSE
  | EQ
  | EOF
  | DOT
  | DISTINCT
  | DESC
  | DELETE
  | CREATE
  | COMMA
  | BY
  | BAR
  | ASC
  | ANY
  | AND
  | ALL

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState138
  | MenhirState131
  | MenhirState126
  | MenhirState120
  | MenhirState116
  | MenhirState110
  | MenhirState108
  | MenhirState102
  | MenhirState87
  | MenhirState83
  | MenhirState80
  | MenhirState79
  | MenhirState75
  | MenhirState67
  | MenhirState55
  | MenhirState53
  | MenhirState45
  | MenhirState41
  | MenhirState38
  | MenhirState33
  | MenhirState31
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState17
  | MenhirState15
  | MenhirState14
  | MenhirState5
  | MenhirState3
  
open Ast
open Lexing
open Parsing
open Table
open Visualizer

let rec _menhir_goto_vis_method : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.plot) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let plot = _v in
        let ((((_menhir_stack, _), top), _, cols), _, f_tb) = _menhir_stack in
        let _5 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.expr) =                                                                                               ( SelTop(top, cols, f_tb, plot) ) in
        _menhir_goto_statement _menhir_env _menhir_stack _v
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let plot = _v in
        let (((_menhir_stack, _), col), _, f_tb) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.expr) =                                                                           ( Distin(ColName col, f_tb, plot) ) in
        _menhir_goto_statement _menhir_env _menhir_stack _v
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let plot = _v in
        let ((_menhir_stack, _, cols), _, f_tb) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expr) =                                                                        ( SelCol (cols, f_tb, plot)) in
        _menhir_goto_statement _menhir_env _menhir_stack _v
    | _ ->
        _menhir_fail ()

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.c_tree) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LPAREN ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.c_tree) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAREN ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.plot) =                  ( BarGraph ) in
        _menhir_goto_vis_method _menhir_env _menhir_stack _menhir_s _v
    | HISTOG ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.plot) =                  ( Hist2d ) in
        _menhir_goto_vis_method _menhir_env _menhir_stack _menhir_s _v
    | LINE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.plot) =                  ( LineGraph ) in
        _menhir_goto_vis_method _menhir_env _menhir_stack _menhir_s _v
    | SCATT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.plot) =                  ( Scatter2d ) in
        _menhir_goto_vis_method _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_cond_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.c_tree) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, c) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.c_tree) =                                    ( c ) in
            _menhir_goto_cond_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR | ORDER | PLOT | RPAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, left), _, right) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.c_tree) =                                              ( (Or  (left,right)) ) in
            _menhir_goto_cond_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, left), _, right) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.c_tree) =                                              ( (And (left,right)) ) in
        _menhir_goto_cond_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, tb), _, pairs), _, conds) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                       (Update (conds,pairs,TbName tb) ) in
            _menhir_goto_statement _menhir_env _menhir_stack _v
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
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | ORDER ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BY ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ASC ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, tb), _, conds), col) = _menhir_stack in
                        let _7 = () in
                        let _5 = () in
                        let _4 = () in
                        let _2 = () in
                        let _v : (Ast.expr) =                                                                 ( Where (conds, Sort (ColName col, ASC, TbName tb) ) ) in
                        _menhir_goto_filtered_table _menhir_env _menhir_stack _menhir_s _v
                    | DESC ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, tb), _, conds), col) = _menhir_stack in
                        let _7 = () in
                        let _5 = () in
                        let _4 = () in
                        let _2 = () in
                        let _v : (Ast.expr) =                                                                  ( Where (conds, Sort (ColName col, DESC, TbName tb) ) ) in
                        _menhir_goto_filtered_table _menhir_env _menhir_stack _menhir_s _v
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
        | PLOT | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, tb), _, conds) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                                        ( Where (conds,TbName tb) ) in
            _menhir_goto_filtered_table _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, tb), _, conds) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                     ( Delete (conds,TbName tb) ) in
            _menhir_goto_statement _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_filtered_table : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PLOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _), top), _, cols), _, f_tb) = _menhir_stack in
            let _5 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                             ( SelTop(top, cols, f_tb, VisNone) ) in
            _menhir_goto_statement _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PLOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _), col), _, f_tb) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                          ( Distin(ColName col, f_tb, VisNone) ) in
            _menhir_goto_statement _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PLOT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _, cols), _, f_tb) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                       ( SelCol (cols, f_tb, VisNone)) in
            _menhir_goto_statement _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_value_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.typ list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.typ list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_value_field_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState120 | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.typ list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_value_field__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_cond_field : _menhir_env -> 'ttv_tail -> _menhir_state -> (Table.condition) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let single = _v in
    let _v : (Ast.c_tree) =                          ( (Cond single) ) in
    _menhir_goto_cond_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_pair_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.expr * Ast.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : ((Ast.expr * Ast.typ) list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_pair_field__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : ((Ast.expr * Ast.typ) list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_pair_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_COMMA_col_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState116 | MenhirState87 | MenhirState41 | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_col_field__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_col_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_typ_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.expr * Ast.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : ((Ast.expr * Ast.typ) list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_typ_field_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : ((Ast.expr * Ast.typ) list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_typ_field__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce17 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, tb) = _menhir_stack in
    let _v : (Ast.expr) =              ( TbName tb ) in
    _menhir_goto_filtered_table _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LPAREN ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BY ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASC ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, tb), col) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _2 = () in
                let _v : (Ast.expr) =                                        ( Sort (ColName col, ASC, TbName tb) ) in
                _menhir_goto_filtered_table _menhir_env _menhir_stack _menhir_s _v
            | DESC ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, tb), col) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _2 = () in
                let _v : (Ast.expr) =                                        ( Sort (ColName col, DESC,TbName tb) ) in
                _menhir_goto_filtered_table _menhir_env _menhir_stack _menhir_s _v
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_value_field__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.typ list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let _v : (Ast.typ list) = let vl =
      let xs = xs0 in
          ( xs )
    in
                                               ( vl ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, tb), _, vals) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                   (InsRow (vals,TbName tb)) in
            _menhir_goto_statement _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, tb), _, cols), _, vals) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) =                                                                                                    (InsCol (cols,vals,TbName(tb))) in
            _menhir_goto_statement _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_value_field : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, col), _, vl) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr * Ast.typ) =                                       ( (ColName col, vl) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | SEMICOLON | WHERE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : ((Ast.expr * Ast.typ) list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_pair_field_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
        let _2 = () in
        let _v : (Table.condition) =                                   ( (e1,NE,e2) ) in
        _menhir_goto_cond_field _menhir_env _menhir_stack _menhir_s _v
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
        let _2 = () in
        let _v : (Table.condition) =                                   ( (e1,LT,e2) ) in
        _menhir_goto_cond_field _menhir_env _menhir_stack _menhir_s _v
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
        let _2 = () in
        let _v : (Table.condition) =                                   ( (e1,LE,e2) ) in
        _menhir_goto_cond_field _menhir_env _menhir_stack _menhir_s _v
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
        let _2 = () in
        let _v : (Table.condition) =                                   ( (e1,GT,e2) ) in
        _menhir_goto_cond_field _menhir_env _menhir_stack _menhir_s _v
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
        let _2 = () in
        let _v : (Table.condition) =                                   ( (e1,GE,e2) ) in
        _menhir_goto_cond_field _menhir_env _menhir_stack _menhir_s _v
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
        let _2 = () in
        let _v : (Table.condition) =                                   ( (e1,EQ,e2) ) in
        _menhir_goto_cond_field _menhir_env _menhir_stack _menhir_s _v
    | MenhirState120 | MenhirState110 | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | FLOAT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | INT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.typ list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_value_field_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_col_field__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let _v : (Ast.expr list) = let cols =
      let xs = xs0 in
          ( xs )
    in
                                                ( cols ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FROM ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FROM ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState83 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | JOIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | ON ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | ID _v ->
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
                                    | ID _v ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_stack = (_menhir_stack, _v) in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | EQ ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | ID _v ->
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
                                                    | ID _v ->
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let col2 = _v in
                                                        let (((_menhir_stack, tb1), col1), tb2) = _menhir_stack in
                                                        let _6 = () in
                                                        let _4 = () in
                                                        let _2 = () in
                                                        let _v : (Ast.expr * Ast.expr) =                            ( (Path(TbName tb1, ColName col1),Path(TbName tb2, ColName col2)) ) in
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let j_cond = _v in
                                                        let (((_menhir_stack, _, cols), _, tb1), tb2) = _menhir_stack in
                                                        let _7 = () in
                                                        let _5 = () in
                                                        let _3 = () in
                                                        let _1 = () in
                                                        let _v : (Ast.expr) =                                                                                ( Joins (TbName tb1,TbName tb2, cols, j_cond) ) in
                                                        _menhir_goto_statement _menhir_env _menhir_stack _v
                                                    | _ ->
                                                        assert (not _menhir_env._menhir_error);
                                                        _menhir_env._menhir_error <- true;
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        raise _eRR)
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    raise _eRR)
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            raise _eRR)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        raise _eRR)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
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
                | ORDER ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
                | UNION ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ALL ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | SEL ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | ANY ->
                                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                            | ID _v ->
                                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                            | FROM ->
                                _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
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
                | WHERE ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
                | PLOT | SEMICOLON ->
                    _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FROM ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let tb2 = _v in
                let (((_menhir_stack, _, cols1), _, tb1), _, cols2) = _menhir_stack in
                let _9 = () in
                let _7 = () in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Ast.expr) =                                                                                              ( Union (SelCol (cols1,TbName tb1, VisNone), SelCol(cols2,TbName tb2, VisNone)) ) in
                _menhir_goto_statement _menhir_env _menhir_stack _v
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
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VALUES ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | FALSE ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState120
                    | FLOAT _v ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
                    | INT _v ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
                    | STRING _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
                    | TRUE ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState120
                    | RPAREN ->
                        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState120
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
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
    | _ ->
        _menhir_fail ()

and _menhir_goto_col_field : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANY ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | ID _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | FROM | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.expr list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_col_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_typ_field : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr * Ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : ((Ast.expr * Ast.typ) list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_typ_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_pair_field__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.expr * Ast.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let _v : ((Ast.expr * Ast.typ) list) = let pr =
      let xs = xs0 in
          ( xs )
    in
                                               ( pr ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | WHERE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | LPAREN ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, tb), _, pairs) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expr) =                                             (UpdAll (pairs,TbName tb) ) in
        _menhir_goto_statement _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | FLOAT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_top_field : _menhir_env -> 'ttv_tail -> (Ast.top_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ANY ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | ID _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | FROM ->
        _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ORDER ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
    | WHERE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
    | PLOT | SEMICOLON ->
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.typ list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_value_field__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.typ) =          ( Bool true ) in
    _menhir_goto_value_field _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let s = _v in
    let _v : (Ast.typ) =                ( String s ) in
    _menhir_goto_value_field _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (Ast.typ) =             ( Int i ) in
    _menhir_goto_value_field _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let f = _v in
    let _v : (Ast.typ) =               ( Float f ) in
    _menhir_goto_value_field _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.typ) =           ( Bool false ) in
    _menhir_goto_value_field _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_col_field__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let col = _v in
            let (_menhir_stack, _menhir_s, tb) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                            ( Path(TbName tb, ColName col) ) in
            _menhir_goto_col_field _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | COMMA | FROM | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, col) = _menhir_stack in
        let _v : (Ast.expr) =              ( ColName col ) in
        _menhir_goto_col_field _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) =         ( ColName "*" ) in
    _menhir_goto_col_field _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, s) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) =                              ( s ) in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, e) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) =                   (e) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LPAREN ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | FLOAT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
    | GE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | FLOAT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
    | GT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | FLOAT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | LE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | FLOAT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | LT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | FLOAT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | NE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | FLOAT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_goto_loption_separated_nonempty_list_COMMA_typ_field__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.expr * Ast.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let _v : ((Ast.expr * Ast.typ) list) = let typs =
      let xs = xs0 in
          ( xs )
    in
                                               ( typs ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, tb), _, colsets) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.expr) =                                                                    ( Create (TbName tb, colsets) ) in
        _menhir_goto_statement _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TBOOL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, col) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr * Ast.typ) =                      ( (ColName col, Bool false) ) in
        _menhir_goto_typ_field _menhir_env _menhir_stack _menhir_s _v
    | TFLOAT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, col) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr * Ast.typ) =                      ( (ColName col, Float 0.0) ) in
        _menhir_goto_typ_field _menhir_env _menhir_stack _menhir_s _v
    | TINT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, col) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr * Ast.typ) =                    ( (ColName col, Int 0) ) in
        _menhir_goto_typ_field _menhir_env _menhir_stack _menhir_s _v
    | TSTRING ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, col) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr * Ast.typ) =                       ( (ColName col, String "") ) in
        _menhir_goto_typ_field _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = () in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CREATE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TABLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
                    | RPAREN ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_s = MenhirState131 in
                        let _v : ((Ast.expr * Ast.typ) list) =     ( [] ) in
                        _menhir_goto_loption_separated_nonempty_list_COMMA_typ_field__ _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | DELETE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FROM ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | WHERE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
                    | LPAREN ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
                | SEMICOLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, tb) = _menhir_stack in
                    let _2 = () in
                    let _1 = () in
                    let _v : (Ast.expr) =                           ( DelAll (TbName tb) ) in
                    _menhir_goto_statement _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | INSERT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INTO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ANY ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                    | ID _v ->
                        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                    | RPAREN ->
                        _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
                | VALUES ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | LPAREN ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | FALSE ->
                            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                        | FLOAT _v ->
                            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
                        | INT _v ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
                        | STRING _v ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
                        | TRUE ->
                            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                        | RPAREN ->
                            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | SEL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ANY ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | DISTINCT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState41 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FROM ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
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
        | ID _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | TOP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState41 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | PERCENT ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, i) = _menhir_stack in
                    let _2 = () in
                    let _v : (Ast.top_t) =                      ( TopPercent i) in
                    _menhir_goto_top_field _menhir_env _menhir_stack _v
                | ANY | FROM | ID _ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, i) = _menhir_stack in
                    let _v : (Ast.top_t) =              ( TopNum i ) in
                    _menhir_goto_top_field _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | FROM ->
            _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | UPDATE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
                | SEMICOLON | WHERE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState3 in
                    let _v : ((Ast.expr * Ast.typ) list) =     ( [] ) in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_pair_field__ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)
  


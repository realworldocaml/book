exception Error

type token = 
  | TRUE
  | STRING of (
# 4 "partial_parser.mly"
       (string)
# 9 "partial_parser.ml"
)
  | RIGHT_BRACK
  | RIGHT_BRACE
  | NULL
  | LEFT_BRACK
  | LEFT_BRACE
  | INT of (
# 1 "partial_parser.mly"
       (int)
# 19 "partial_parser.ml"
)
  | ID of (
# 3 "partial_parser.mly"
       (string)
# 24 "partial_parser.ml"
)
  | FLOAT of (
# 2 "partial_parser.mly"
       (float)
# 29 "partial_parser.ml"
)
  | FALSE
  | EOF
  | COMMA
  | COLON

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state

let _eRR =
  Error

let rec exp : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 16 "partial_parser.mly"
       (unit)
# 53 "partial_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_startp = lexbuf.Lexing.lex_start_p;
        _menhir_endp = lexbuf.Lexing.lex_curr_p;
        _menhir_shifted = max_int;
        }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = () in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    ((let _v : (
# 16 "partial_parser.mly"
       (unit)
# 76 "partial_parser.ml"
    ) = 
# 20 "partial_parser.mly"
     ( () )
# 80 "partial_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = _menhir_stack in
    let (_v : (
# 16 "partial_parser.mly"
       (unit)
# 87 "partial_parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_v : (
# 16 "partial_parser.mly"
       (unit)
# 94 "partial_parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
    let (_1 : (
# 16 "partial_parser.mly"
       (unit)
# 101 "partial_parser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv2)) : 'freshtv4)) : 'freshtv6)) : 'freshtv8)) : 'freshtv10))




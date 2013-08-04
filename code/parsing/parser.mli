exception Error

type token = 
  | TRUE
  | STRING of (string)
  | RIGHT_BRACK
  | RIGHT_BRACE
  | NULL
  | LEFT_BRACK
  | LEFT_BRACE
  | INT of (int)
  | ID of (string)
  | FLOAT of (float)
  | FALSE
  | EOF
  | COMMA
  | COLON


val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Json.value option)
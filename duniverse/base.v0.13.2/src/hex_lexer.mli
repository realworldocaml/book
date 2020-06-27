type result =
  | Neg of string
  | Pos of string

val parse_hex : Lexing.lexbuf -> result

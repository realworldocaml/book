(** This module helps combining {!Lexer} and {!Parser}. *)

val process :
  lexer_init:('a -> 'lexbuf) ->
  lexer_fun:('lexbuf -> 'token) ->
  parser_fun:(('lexbuf -> 'token) -> 'lexbuf -> 'ast) ->
  input:'a ->
  'ast
(** [process lexer_init lexer_fun parser_fun input] initializes a lexer,
    and composes it with a parser in order to transform an input text into
    an abstract syntax tree. *)

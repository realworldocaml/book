(** Entry points in the parser *)

val implementation : Lexing.lexbuf -> Parsetree.structure_item list
(** Parse a structure *)

val interface : Lexing.lexbuf -> Parsetree.signature_item list
(** Parse a signature *)

val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase
(** Parse a toplevel phrase *)

val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
(** Parse a series of toplevel phrases *)

val core_type : Lexing.lexbuf -> Parsetree.core_type
(** Parse a core type *)

val expression : Lexing.lexbuf -> Parsetree.expression
(** Parse an expression *)

val pattern : Lexing.lexbuf -> Parsetree.pattern
(** Parse a pattern *)

(** Text wrapping and filling for OCaml. *)

type t = {
  width : int;
  initial_indent : string;
  subsequent_indent : string;
  expand_tabs : bool;
  replace_whitespace : bool;
  fix_sentence_endings : bool;
  break_long_words : bool;
  break_on_hyphens : bool;
  drop_whitespace : bool
}

val make : ?initial_indent:string ->
  ?subsequent_indent:string ->
  ?expand_tabs:bool ->
  ?replace_whitespace:bool ->
  ?fix_sentence_endings:bool ->
  ?break_long_words:bool ->
  ?break_on_hyphens:bool ->
  ?drop_whitespace:bool ->
  int -> t

val wrap : t -> string -> string list
val fill : t -> string -> string

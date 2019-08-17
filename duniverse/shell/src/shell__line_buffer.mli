(** String buffers that automatically get flushed at every line return. *)

type t

(** [create ~eol f]
    Create a new line buffer where f will be called once on every line.
    Eol is the endline character (it's possible to use a Linebuffer to
    process null separated strings )
*)
val create : ?eol:char -> (string -> unit) -> t

(**
   [flush b] Flushes any pending output to the callback function.
   This causes unfinished newlines to be flushed out so adding
   more characters after flushing might result in there looking as though
   there are more lines than there really were.
*)
val flush : t -> unit

val add_char : t -> char -> unit
val add_string : t -> string -> unit

(** [add_substring b s ofs len] takes [len] characters from offset [ofs] in
    string [s] and appends them at the end of the buffer [b].
*)
val add_substring : t -> string  -> pos:int -> len:int -> unit
val add_subbytes  : t -> Bytes.t -> pos:int -> len:int -> unit

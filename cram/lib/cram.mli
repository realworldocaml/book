(** Library to handle {{:https://bitheap.org/cram/}cram tests}
    format. *)

(** The type for line items. *)
type item = [
  | `Output  of string
  | `Command of string
  | `Comment of string
  | `Part    of string
]

val pp_item: item Fmt.t

type t = item list
(** The type for cram files. *)

val pp: t Fmt.t
val to_string: t -> string

val parse_file: string -> t
val parse_lexbuf: Lexing.lexbuf -> item list

val run: string -> f:(string -> t -> string) -> unit
(** [run n f] runs the expect callback [f] over the file named
    [n]. [f] is called with the raw contents of [n] and its structured
    contents; it returns the new file contents. If the result of [f] is
    different from the initial contents, then [$n.corrected] is created
    with the new contents. *)

val part: string -> t -> t option
(** [part i t] is the i-th part in [t]. *)

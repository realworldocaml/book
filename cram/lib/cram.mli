(** Library to handle {{:https://bitheap.org/cram/}cram tests}
    format. *)

(** The type for output lines. *)
type output = [`Output of string | `Ellipsis | `Exit_code of int]

(** The type for all lines. *)
type line = [
  output
  | `Command of string
  | `Comment of string
  | `Part    of string
  | `Exit_code of int
  | `Non_det of [`Output | `Command]
]

val pp_line: ?hide:bool -> line Fmt.t
(** [pp_line] is the pretty-printer for lines. If [hide] is true,
    commands starting by [@@] are not displayed. By default, [hide] is
    not set (all the lines are printed). *)

(** The type for tests. *)
type test = {
  part: string option;
  non_deterministic: [`Command | `Output | `False];
  command: string;
  output: output list;
  exit_code: int;
  lines: line list;
}

(** The type for test items *)
type item =
  | Test of test
  | Line of line

type t = item list [@@deriving sexp]
(** The type for cram files. *)

val pp: ?hide:bool -> t Fmt.t
val to_string: ?hide:bool -> t -> string

val parse_file: string -> t
val parse_lexbuf: Lexing.lexbuf -> t

val run: string -> f:(string -> t -> string) -> unit
(** [run n f] runs the expect callback [f] over the file named
    [n]. [f] is called with the raw contents of [n] and its structured
    contents; it returns the new file contents. If the result of [f] is
    different from the initial contents, then [$n.corrected] is created
    with the new contents. *)

val part: string -> t -> t option
(** [part i t] is the i-th part in [t]. *)

val pp_exit_code: int Fmt.t
(** Display exit code. *)

val equal_output: output list -> output list -> bool
(** [equal x y] compares [x] and [y]; {Ellipsis} are used as
    wildcards for zero, one or multiple matching lines. *)

val to_html: t -> string

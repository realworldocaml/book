(** Overrides the Location module of OCaml *)

(** There are less functions in this module. However the API should be more
    stable than the Location module of OCaml. *)

open! Import

type t = location = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
  loc_ghost : bool;
}

val in_file : string -> t
(** Return an empty ghost range located in a given file. *)

val set_filename : t -> string -> t
(** Set the [pos_fname] both in [loc_start] and [loc_end]. Leave the rest as is. *)

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)

val init : Lexing.lexbuf -> string -> unit
(** Set the file name and line number of the [lexbuf] to be the start of the
    named file. *)

val raise_errorf : ?loc:t -> ('a, Caml.Format.formatter, unit, 'b) format4 -> 'a
(** Raise a located error. Should be avoided as much as possible, in favor of
    {!error_extensionf}. *)

val of_lexbuf : Lexing.lexbuf -> t
(** Return the location corresponding to the last matched regular expression *)

val report_exception : Caml.Format.formatter -> exn -> unit
(** Report an exception on the given formatter *)

val print : Caml.Format.formatter -> t -> unit
(** Prints [File "...", line ..., characters ...-...:] *)

type nonrec 'a loc = 'a loc = { txt : 'a; loc : t }

val compare_pos : Lexing.position -> Lexing.position -> int
val min_pos : Lexing.position -> Lexing.position -> Lexing.position
val max_pos : Lexing.position -> Lexing.position -> Lexing.position
val compare : t -> t -> int

module Error : sig
  type location = t
  type t

  val make : loc:location -> string -> sub:(location * string) list -> t

  val createf :
    loc:location -> ('a, Caml.Format.formatter, unit, t) format4 -> 'a

  val message : t -> string
  val set_message : t -> string -> t

  val register_error_of_exn : (exn -> t option) -> unit
  (** Register an exception handler. Exception registered this way will be
      properly displayed by [report_exception]. *)

  val of_exn : exn -> t option

  val to_extension : t -> extension
  (** Convert an error to an extension point. The compiler recognizes this and
      displays the error properly. *)

  val raise : t -> 'a
  (** Raise a compiler [Parsing.Location.Error] exception. The composition of
      [Location.Error.createf] with [Location.Error.raise] is the same as
      [Location.raise_errorf]. *)

  val update_loc : t -> location -> t
  (** Update where the error is located. The old location will be overwritten. *)

  val get_location : t -> location
  (** Find out where the error is located. *)
end
with type location := t

val error_extensionf :
  loc:t -> ('a, Format.formatter, unit, extension) format4 -> 'a
(** Returns an error extension node. When encountered in the AST, the compiler
    recognizes it and displays the error properly. *)

exception Error of Error.t

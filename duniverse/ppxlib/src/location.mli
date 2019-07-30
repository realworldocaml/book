(** Overrides the Location module of OCaml *)

(** There are less functions in this module. However the API should be more stable than
    the Location module of OCaml. *)

open! Import

type t = location =
  { loc_start : Lexing.position
  ; loc_end   : Lexing.position
  ; loc_ghost : bool
  }

(** Return an empty ghost range located in a given file. *)
val in_file : string -> t

(** An arbitrary value of type [t]; describes an empty ghost range. *)
val none : t

(** Raise a located error. The exception is caught by driver and handled
    appropriately *)
val raise_errorf : ?loc:t -> ('a, Caml.Format.formatter, unit, 'b) format4 -> 'a

(** Return the location corresponding to the last matched regular expression *)
val of_lexbuf : Lexing.lexbuf -> t

(** Report an exception on the given formatter *)
val report_exception : Caml.Format.formatter -> exn -> unit

(** Prints [File "...", line ..., characters ...-...:] *)
val print : Caml.Format.formatter -> t -> unit

type nonrec 'a loc = 'a loc =
  { txt : 'a
  ; loc : t
  }

module Error : sig
  type location = t
  type t

  val make : loc:location -> string -> sub:(location * string) list -> t
  val createf : loc:location -> ('a, unit, string, t) format4 -> 'a

  val message : t -> string
  val set_message : t -> string -> t

  (** Register an exception handler. Exception registered this way will be properly
      displayed by [report_exception]. *)
  val register_error_of_exn: (exn -> t option) -> unit

  val of_exn : exn -> t option

  (** Convert an error to an extension point. The compiler recognizes this and displays
      the error properly. *)
  val to_extension : t -> extension
end with type location := t

exception Error of Error.t

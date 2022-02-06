(** Source code locations (ranges of positions), used in parsetrees *)

type t = Ocaml_common.Location.t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
  loc_ghost : bool;
}
(** The location type *)

type 'a loc = 'a Ocaml_common.Location.loc = { txt : 'a; loc : t }
(** A located type *)

module Error : sig
  type location

  type t
  (** The location error type. It contains a located main message and a
      (possibly empty) list of located submessages. *)

  val is_well_formed : t -> bool
  (** A location error constructed via [make] is always well-formed. A malformed
      location error is a value of type [location_report] on compilers >= 4.08,
      whose [kind] is different from [Report_error]. Notice that
      [location_report] does not explicitly form part of Astlib. *)

  val main_msg : t -> string loc
  (** Get the located error main message. *)

  val sub_msgs : t -> string loc list
  (** Get the located error sub-messages. *)

  val set_main_msg : t -> string -> t
  (** Set the text of the error's main message. The location stays as is. *)

  val set_main_loc : t -> location -> t
  (** Set the location of the error's main message. The text satys as is. *)

  val make : sub:string loc list -> string loc -> t
  (** Construct a location error. *)

  val of_exn : exn -> t option
  (** Turn an exception into a location error, if possible. *)
end
with type location := t

val set_input_name : string -> unit
(** Set the name of the input source, e.g. the file name. *)

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)

(** {1 Automatically reporting errors for raised exceptions} *)

val register_error_of_exn : (exn -> Error.t option) -> unit
(** Each compiler module which defines a custom type of exception which can
    surface as a user-visible error should register a "printer" for this
    exception using [register_error_of_exn]. The result of the printer is an
    [error] value containing a location, a message, and optionally sub-messages
    (each of them being located as well). *)

exception Error of Error.t
(** Located exception. *)

val raise_errorf : ?loc:t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** Raise a located exception. *)

val report_exception : Format.formatter -> exn -> unit
(** Report an exception on the given formatter *)

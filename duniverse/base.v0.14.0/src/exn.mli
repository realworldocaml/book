(** Exceptions.

    [sexp_of_t] uses a global table of sexp converters.  To register a converter for a new
    exception, add [[@@deriving sexp]] to its definition. If no suitable converter is
    found, the standard converter in [Printexc] will be used to generate an atomic
    S-expression. *)

open! Import

type t = exn [@@deriving_inline sexp_of]

val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

[@@@end]

include Pretty_printer.S with type t := t

(** Raised when finalization after an exception failed, too.
    The first exception argument is the one raised by the initial
    function, the second exception the one raised by the finalizer. *)
exception Finally of t * t

exception Reraised of string * t

(** [create_s sexp] returns an exception [t] such that [phys_equal (sexp_of_t t) sexp].
    This is useful when one wants to create an exception that serves as a message and the
    particular exn constructor doesn't matter. *)
val create_s : Sexp.t -> t

(** Same as [raise], except that the backtrace is not recorded. *)
val raise_without_backtrace : t -> _

val reraise : t -> string -> _

(** Types with [format4] are hard to read, so here's an example.

    {[
      let foobar str =
        try
          ...
        with exn ->
          Exn.reraisef exn "Foobar is buggy on: %s" str ()
    ]} *)
val reraisef : t -> ('a, unit, string, unit -> _) format4 -> 'a


(** Human-readable, multi-line. *)
val to_string : t -> string

(** Machine format, single-line. *)
val to_string_mach : t -> string

(** Executes [f] and afterwards executes [finally], whether [f] throws an exception or
    not. *)
val protectx : f:('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b

val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a


(** [handle_uncaught ~exit f] catches an exception escaping [f] and prints an error
    message to stderr.  Exits with return code 1 if [exit] is [true], and returns unit
    otherwise.

    Note that since OCaml 4.02.0, you don't need to use this at the entry point of your
    program, as the OCaml runtime will do better than this function. *)
val handle_uncaught : exit:bool -> (unit -> unit) -> unit

(** [handle_uncaught_and_exit f] returns [f ()], unless that raises, in which case it
    prints the exception and exits nonzero. *)
val handle_uncaught_and_exit : (unit -> 'a) -> 'a

(** Traces exceptions passing through.  Useful because in practice, backtraces still don't
    seem to work.

    Example:
    {[
      let rogue_function () = if Random.bool () then failwith "foo" else 3
      let traced_function () = Exn.reraise_uncaught "rogue_function" rogue_function
                                 traced_function ();;
    ]}
    {v : Program died with Reraised("rogue_function", Failure "foo") v} *)
val reraise_uncaught : string -> (unit -> 'a) -> 'a


(** [does_raise f] returns [true] iff [f ()] raises, which is often useful in unit
    tests. *)
val does_raise : (unit -> _) -> bool

(** User code never calls this.  It is called in [std_kernel.ml] as a top-level side
    effect to change the display of exceptions and install an uncaught-exception
    printer. *)
val initialize_module : unit -> unit

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val clear_backtrace : unit -> unit
end

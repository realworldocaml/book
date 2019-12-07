open! Import

type 'a t = 'a -> unit

type 'a inv = 'a t

module type S = sig
  type t
  val invariant : t inv
end

module type S1 = sig
  type 'a t
  val invariant : 'a inv -> 'a t inv
end

module type S2 = sig
  type ('a, 'b) t
  val invariant : 'a inv -> 'b inv -> ('a, 'b) t inv
end

module type S3 = sig
  type ('a, 'b, 'c) t
  val invariant : 'a inv -> 'b inv -> 'c inv -> ('a, 'b, 'c) t inv
end

module type Invariant = sig

  (** This module defines signatures that are to be included in other signatures to ensure
      a consistent interface to invariant-style functions.  There is a signature ([S],
      [S1], [S2], [S3]) for each arity of type.  Usage looks like:

      {[
        type t
        include Invariant.S with type t := t
      ]}

      or

      {[
        type 'a t
        include Invariant.S1 with type 'a t := 'a t
      ]}
  *)

  type nonrec 'a t = 'a t

  module type S  = S
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3

  (** [invariant here t sexp_of_t f] runs [f ()], and if [f] raises, wraps the exception
      in an [Error.t] that states "invariant failed" and includes both the exception
      raised by [f], as well as [sexp_of_t t].  Idiomatic usage looks like:

      {[
        invariant [%here] t [%sexp_of: t] (fun () ->
          ... check t's invariants ... )
      ]}

      For polymorphic types:

      {[
        let invariant check_a t =
          Invariant.invariant [%here] t [%sexp_of: _ t] (fun () -> ... )
      ]}

      It's okay to use [ [%sexp_of: _ t] ] because the exceptions raised by [check_a] will
      show the parts that are sexp_opaque at top-level. *)
  val invariant
    :  Source_code_position0.t
    -> 'a
    -> ('a -> Sexp.t)
    -> (unit -> unit)
    -> unit

  (** [check_field] is used when checking invariants using [Fields.iter].  It wraps an
      exception raised when checking a field with the field's name.  Idiomatic usage looks
      like:

      {[
        type t =
          { foo : Foo.t;
            bar : Bar.t;
          }
        [@@deriving_inline fields][@@@end]

        let invariant t : unit =
          Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
            let check f = Invariant.check_field t f in
            Fields.iter
              ~foo:(check Foo.invariant)
              ~bar:(check Bar.invariant))
        ;;
      ]} *)
  val check_field : 'a -> 'b t -> ('a, 'b) Field.t -> unit
end

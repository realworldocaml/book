(** This module defines signatures that extend [Core_kernel.Invariant] with an [Async]
    submodule for invariants that use async computation and return [unit Deferred.t]. *)

open! Core_kernel
open! Import

module Async = struct
  type 'a t = 'a -> unit Deferred.t
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
end

module type Async_invariant = sig
  include module type of Core_kernel.Invariant

  module Async : sig
    open Async

    type nonrec 'a t = 'a Async.t

    module type S = S
    module type S1 = S1
    module type S2 = S2
    module type S3 = S3

    val invariant
      :  Source_code_position.t
      -> 'a
      -> ('a -> Sexp.t)
      -> (unit -> unit Deferred.t)
      -> unit Deferred.t

    (** [check_field] can be used to check record fields when using [[@@deriving fields]].
        Idiomatic usage looks like:

        {[
          type t = { foo : Foo.t ; bar : Bar.t }
          [@@deriving fields]

          let invariant t =
            Invariant.Async.invariant [%here] t [%sexp_of: t] (fun () ->
              let check inv = Invariant.Async.check_field t inv in
              Fields.fold ~init:(return ())
                ~foo: (check Foo.invariant)
                ~bar: (check Bar.invariant) ]}

        When some fields have synchronous invariants, or do not need to be checked, it
        may be useful to define a second wrapper around [check_field]:

        {[
          type t = { foo : Foo.t ; bar : Bar.t ; quux : Quux.t }
          [@@deriving fields]

          let invariant t =
            Invariant.Async.invariant [%here] t [%sexp_of: t] (fun () ->
              let check' inv = Invariant.Async.check_field t inv in
              let check inv = check' (fun x -> inv x; return ()) in
              Fields.fold ~init:(return ())
                ~foo:  (check' Foo.invariant)
                ~bar:  (check  Bar.invariant)
                ~quux: (check  ignore) ]} *)
    val check_field
      :  'a
      -> 'b t
      -> unit Deferred.t
      -> ('a, 'b) Field.t
      -> unit Deferred.t
  end
end

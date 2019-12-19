open! Core_kernel
open! Async_kernel

module type Quickcheck_async_configured = sig
  include Quickcheck.Quickcheck_configured

  (** Like [test], but for asynchronous tests. *)
  val async_test
    :  ?seed:Quickcheck.seed
    -> ?trials:int
    -> ?sexp_of:('a -> Sexp.t)
    -> 'a Quickcheck.Generator.t
    -> f:('a -> unit Deferred.t)
    -> unit Deferred.t
end

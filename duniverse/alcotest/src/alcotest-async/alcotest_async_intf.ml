module type V1 = sig
  include
    Alcotest_engine.V1.Cli.S with type return = unit Async_kernel.Deferred.t

  val test_case :
    ?timeout:Core_kernel.Time.Span.t ->
    string ->
    Alcotest.speed_level ->
    ('a -> unit Async_kernel.Deferred.t) ->
    'a test_case

  val test_case_sync :
    string -> Alcotest.speed_level -> ('a -> unit) -> 'a test_case
end

module type Alcotest_async = sig
  include V1

  (** {1 Versioned APIs} *)

  module V1 : V1
  (** An alias of the above API that provides a stability guarantees over major
      version changes. *)
end

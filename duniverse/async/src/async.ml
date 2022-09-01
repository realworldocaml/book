open! Async_kernel

(** {2 Async_kernel} *)

include Async_kernel (** @open *)

module Deferred = struct
  include Deferred

  module Or_error = struct
    include Async_kernel.Deferred.Or_error
    module Expect_test_config = Deferred_or_error_expect_test_config
  end
end

(** {2 Async_unix} *)

include Async_unix (** @open *)

(** {2 Async_command} *)

(* We define [Command] using [struct include ... end] rather than as an alias so that we
   don't have to add [async_command] to downstream jbuild library imports. *)
module Command = struct
  include Async_command
end

(** {2 Async_rpc} *)

include Async_rpc (** @open *)

(* We define [Quickcheck] using [struct include ... end] rather than as an alias so that
   we don't have to add [async_quickcheck] to downstream jbuild library imports. *)
module Quickcheck = struct
  include Async_quickcheck
end

let%test "Async library initialization does not initialize the scheduler" =
  Scheduler.is_ready_to_initialize ()
;;

module Expect_test_config = Expect_test_config
module Expect_test_config_or_error = Expect_test_config_or_error
module Ppx_log_syntax = Ppx_log_syntax

module Upon_unreleasable_issue = struct
  type t =
    [ `CR (** Leaves a CR, so that features cannot be released. *)
    | `Warning_for_collector_testing (** Only for ppx_expect testing; do not use. *)
    ]
end

module type S = sig
  module IO_run : sig
    type 'a t
  end

  module IO_flush : sig
    type 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val to_run : 'a t -> 'a IO_run.t
  end

  (** Flush whatever need to be to get pending output out on file descriptor 0. *)
  val flush : unit -> unit IO_flush.t

  (** Run an IO operation until completion *)
  val run : (unit -> unit IO_run.t) -> unit

  (** Synchronous check that there is no pending output on file description 0. With async,
      there is no guarantee that on the rhs of a [IO.bind (flush ()) ...] the output is
      completely flushed, that's why we need this. *)
  val flushed : unit -> bool


  (** [upon_unreleasable_issue] specifies how to deal with output that should not be
      released even if it is accepted (e.g. backtraces). The default is [`CR].  *)
  val upon_unreleasable_issue : Upon_unreleasable_issue.t
end

(** Configuration for running expect tests *)
module type Expect_test_config_types = sig
  (** To configure expect_test, add the following at the top of your .ml file, or in some
      import.ml:

      {[
        module Expect_test_config = struct
          include Expect_test_config
          let pre_redirect_hook () = ...
        end
      ]}

      Note that since all expect test are also inline tests, the inline test configuration
      also applies to all expect test.
  *)

  module Upon_unreleasable_issue : sig
    include module type of Upon_unreleasable_issue

    val equal : t -> t -> bool
    val comment_prefix : t -> string

    (** Message to print when an expectation contains a backtrace *)
    val message_when_expectation_contains_backtrace : t -> string
  end

  module type S = S
end

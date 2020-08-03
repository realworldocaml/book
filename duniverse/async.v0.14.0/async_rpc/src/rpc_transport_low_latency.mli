(** This module implements a RPC transport optimized for low-latency. *)

open! Core
open! Import

module Config : sig
  type t [@@deriving sexp]

  (** - [max_message_size] is the maximum message size a reader/writer will accept to
      receive/send.

      - [max_buffer_size] is the maximum size the internal reader/writer's buffer will
        ever grow.

      - [write_timeout] is the maximum time allowed for a write operation to complete
        before an error is reported.

      - When the writer's internal buffer is filled at [buffering_threshold_in_bytes] or
        more, the writer will try to flush its buffer immediately. This is to get good
        latency and avoid buffering too much when sending big batches of messages.

      - If the application hasn't sent any messages in the current Async job and nothing
        is buffered, the first [start_batching_after_num_messages] messages will be sent
        immediately. After that the writer will start buffering. This is to give good
        latency when the application sends a few messages occasionally but still get good
        throughput when sending a batch of messages.

      Note that [start_batching_after_num_messages] and [buffering_threshold_in_bytes]
      have somewhat opposite meanings: the former determines when to start batching and
      the latter determines when to write data that has been batched.
  *)
  val create
    :  ?max_message_size:int (** default Int.max_value *)
    -> ?initial_buffer_size:int (** default 64 KB *)
    -> ?max_buffer_size:int (** default Int.max_value *)
    -> ?write_timeout:Time_ns.Span.t (** default 2 minutes *)
    -> ?buffering_threshold_in_bytes:int (** default 32 KB *)
    -> ?start_batching_after_num_messages:int (** default 2 *)
    -> unit
    -> t
end

(** All the following [create] functions take a [max_message_size] argument in addition to
    a [Config.t]. The final [max_message_size] will be the min of both.

    The rationale for this is that [max_message_size] is more a property of the protocol
    and should be specified by the programmer, while other configuration parameters are
    for tuning purposes.
*)

module Reader : sig
  include module type of struct
    include Rpc_kernel.Transport.Reader
  end

  val create : ?config:Config.t -> max_message_size:int -> Fd.t -> t
end

module Writer : sig
  include module type of struct
    include Rpc_kernel.Transport.Writer
  end

  val create : ?config:Config.t -> max_message_size:int -> Fd.t -> t
end

include module type of struct
  include Rpc_kernel.Transport
end
with module Reader := Rpc_kernel.Transport.Reader
with module Writer := Rpc_kernel.Transport.Writer

val create : ?config:Config.t -> max_message_size:int -> Fd.t -> t

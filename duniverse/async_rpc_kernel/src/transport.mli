(** RPC transport layer

    A transport is a way to send and receive messages.  It is split between a [Reader] and
    a [Writer] part.

    [Async_rpc_kernel] only provides an async [Pipe.t] transport.
    [Async_extra] provides a unix transport based on [Async_unix]'s
    [Reader] and [Writer] modules.
*)

open! Core_kernel
open! Async_kernel

(** Binary headers containing message lengths.  All transports should use this to ensure
    binary compatibility. *)
module Header : sig
  val length : int

  val unsafe_get_payload_length : Bigstring.t -> pos:int -> int
  val unsafe_set_payload_length : Bigstring.t -> pos:int -> int -> unit
end

module Handler_result = Transport_intf.Handler_result

module Reader : sig
  module type S = Transport_intf.Reader
  include S

  val pack : (module S with type t = 'a) -> 'a -> t

  (** Convenience function to wait for the first message and un-bin_prot it.

      Async RPC uses this to handle the handshake at the beginning of the message
      stream. *)
  val read_one_message_bin_prot
    :  t
    -> 'a Bin_prot.Type_class.reader
    -> ('a, [ `Closed | `Eof ]) Result.t Deferred.t
end

module Send_result = Transport_intf.Send_result

module Writer : sig
  module type S = Transport_intf.Writer
  include S

  val pack : (module S with type t = 'a) -> 'a -> t

  (** [can_send t = not (is_closed t || Deferred.is_determined (stopped t))] *)
  val can_send : t -> bool

  (** This function is similar to [Async_unix.Writer.transfer], except that it doesn't
      wait on [Pipe.upstream_flushed] when the writer is closed. *)
  val transfer
    :  t
    -> ?max_num_values_per_read:int (* default: 1_000 *)
    -> 'a Pipe.Reader.t
    -> ('a -> unit)
    -> unit Deferred.t
end

type t =
  { reader : Reader.t
  ; writer : Writer.t
  }
[@@deriving sexp_of]

(** Closes both parts of the transport. *)
val close : t -> unit Deferred.t

open! Core_kernel
open Async_kernel

module Kind : sig
  type 'a t
  val string    : String.t    t
  val bigstring : Bigstring.t t
end

(** Create an rpc [Transport.t] using async pipes.
    The resulting transport will not perform any buffering. It will always write to
    the pipe writer without push-back and always be considered flushed. *)
val create : 'a Kind.t -> 'a Pipe.Reader.t -> 'a Pipe.Writer.t -> Transport.t

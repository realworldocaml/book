open! Core
open! Import

(*_ The [Reader] and [Writer] modules from [Async], renamed to avoid conflicting with
  [Reader] and [Writer] below. *)
module Async_reader = Reader
module Async_writer = Writer

module Reader : sig
  include module type of struct
    include Rpc_kernel.Transport.Reader
  end

  val of_reader : max_message_size:int -> Async_reader.t -> t
end

module Writer : sig
  include module type of struct
    include Rpc_kernel.Transport.Writer
  end

  val of_writer : max_message_size:int -> Async_writer.t -> t
end

include module type of struct
  include Rpc_kernel.Transport
end
with module Reader := Rpc_kernel.Transport.Reader
with module Writer := Rpc_kernel.Transport.Writer

val of_reader_writer : max_message_size:int -> Async_reader.t -> Async_writer.t -> t

val of_fd
  :  ?buffer_age_limit:Async_writer.buffer_age_limit
  -> ?reader_buffer_size:int
  -> max_message_size:int
  -> Fd.t
  -> t

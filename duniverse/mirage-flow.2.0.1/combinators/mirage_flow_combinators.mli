(*
 * Copyright (C) 2016-present David Scott <dave.scott@docker.com>
 * Copyright (c) 2011-present Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-present Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** {1 Flow-related devices using lwt}

    This module define flow-related devices for MirageOS, using lwt for I/O.

    {e Release %%VERSION%% } *)

(** [CONCRETE] expose the private row as [`Msg str] errors, using
    [pp_error] and [pp_write_error]. *)
module type CONCRETE =  Mirage_flow.S
  with type error = [ `Msg of string ]
   and type write_error = [ Mirage_flow.write_error | `Msg of string ]

(** Functor to transform a {{!S}flow} signature using private rows for
    errors into concrete error types. *)
module Concrete (S: Mirage_flow.S): CONCRETE with type flow = S.flow

(** {1 Shutdownable flows} *)
module type SHUTDOWNABLE = sig
  include Mirage_flow.S

  val shutdown_write: flow -> unit Lwt.t
  (** Close the [write] direction of the flow, flushing any buffered
      data and causing future calls to [read] by the peer to return
      [`Eof]. *)

  val shutdown_read: flow -> unit Lwt.t
  (** Close the [read] direction of the flow, such that future calls
      to [write] by the peer will return [`Eof] *)
end

module Copy (Clock: Mirage_clock.MCLOCK) (A: Mirage_flow.S) (B: Mirage_flow.S): sig

  type error = [`A of A.error | `B of B.write_error]
  (** The type for copy errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] pretty-prints errors. *)

  val copy: src:A.flow -> dst:B.flow -> (Mirage_flow.stats, error) result Lwt.t
  (** [copy source destination] copies data from [source] to
      [destination] using the clock to compute a transfer rate. On
      successful completion, some statistics are returned. On failure we
      return a printable error. *)

end

module Proxy (Clock: Mirage_clock.MCLOCK) (A: SHUTDOWNABLE) (B: SHUTDOWNABLE):
sig

  type error
  (** The type for proxy errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] pretty-prints errors. *)

  val proxy: A.flow -> B.flow ->
    ((Mirage_flow.stats * Mirage_flow.stats), error) result Lwt.t
  (** [proxy a b] proxies data between [a] and [b] until both
      sides close. If either direction encounters an error then so
      will [proxy]. If both directions succeed, then return I/O
      statistics. *)

end

module F: sig

  (** In-memory, function-based flows. *)

  include Mirage_flow.S

  type refill = Cstruct.t -> int -> int -> int Lwt.t
  (** The type for refill functions. *)

  val make:
    ?close:(unit -> unit Lwt.t) ->
    ?input:refill ->
    ?output:refill ->
    unit -> flow
  (** [make ~close ~input ~output ()] is a flow using [input] to
      refill its internal input buffer when needed and [output] to
      refill its external output buffer. It is using [close] to
      eventually clean-up other resources on close. *)

  (** {1 String flows} *)

  val input_string: string -> refill
  (** [input_string buf] is the refill function reading its inputs
      from the string [buf]. *)

  val output_bytes: bytes -> refill
  (** [output_bytes buf] is the refill function writing its outputs in
      the buffer [buf]. *)

  val string: ?input:string -> ?output:bytes -> unit -> flow
  (** The flow built using {!input_string} and {!output_bytes}. *)

  val input_strings: string list -> refill
  (** [input_strings bufs] is the refill function reading its inputs
      from the list of buffers [bufs]. Empty strings are ignored. *)

  val output_bytess: bytes list -> refill
  (** [output_bytess buf] is the refill function writing its outputs in
      the list of buffers [buf]. Empty strings are ignored. *)

  val strings: ?input:string list -> ?output:bytes list -> unit -> flow
  (** The flow built using {!input_strings} and {!output_bytess}. *)

  (** {1 Cstruct buffers flows} *)

  val input_cstruct: Cstruct.t -> refill
  (** Same as {!input_string} but for {!Cstruct.t} buffers. *)

  val output_cstruct: Cstruct.t -> refill
  (** Same as {!output_string} buf for {!Cstruct.t} buffers. *)

  val cstruct: ?input:Cstruct.t -> ?output:Cstruct.t -> unit -> flow
  (** Same as {!string} but for {!Cstruct.t} buffers. *)

  val input_cstructs: Cstruct.t list -> refill
  (** Same as {!input_strings} but for {!Cstruct.t} buffers. *)

  val output_cstructs: Cstruct.t list -> refill
  (** Same as {!output_strings} but for {!Cstruct.t} buffers. *)

  val cstructs: ?input:Cstruct.t list -> ?output:Cstruct.t list -> unit -> flow
  (** Same as {!strings} but for {!Cstruct.t} buffers. *)

end

type t
(** The type for first-class flows. *)

include Mirage_flow.S with type flow = t

val create: (module Mirage_flow.S with type flow = 'a) -> 'a -> string -> t
(** [create (module M) t name] is the flow representing [t] using the
    function defined in [M]. *)

val pp: t Fmt.t
(** [pp] is the pretty-printer for IO flows. *)

val forward: ?verbose:bool -> src:t -> dst:t -> unit Lwt.t
(** [forward ?verbose ~src ~dst] forwards writes from [src] to
    [dst]. Block until either [src] or [dst] is closed. If [verbose]
    is set (by default it is not), show the full flow contents in the debug
    messages. *)

val proxy: ?verbose:bool -> t -> t -> unit Lwt.t
(** [proxy ?verbose x y] is the same as [forward x y <*> forward y
    x]. Block until both flows are closed. If [verbose] is set (by
    default it is not), show the full flow contents in the debug
    messages. *)

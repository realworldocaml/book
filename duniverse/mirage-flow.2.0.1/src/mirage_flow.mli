(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
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

(** {1 Flow-related signatures}

    This module defines the flow signature for MirageOS.

    {e Release %%VERSION%% } *)

type write_error = [ `Closed ]
(** The type for generic write errors on flows. *)

val pp_write_error: write_error Fmt.t
(** [pp_write_error] is the pretty-printer for write errors. *)

type 'a or_eof = [`Data of 'a | `Eof ]
(** The type for read results on flows. *)

val pp_or_eof: 'a Fmt.t -> 'a or_eof Fmt.t
(** [pp_or_eof] is the pretty-printer for {!or_eof} values. *)

(** Abstract flow signature. *)
module type S = sig

  type error
  (** The type for flow errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type nonrec write_error = private [> write_error ]
  (** The type for write errors. *)

  val pp_write_error: write_error Fmt.t
  (** [pp_write_error] is the pretty-printer for write errors. *)

  type flow
  (** The type for flows. A flow represents the state of a single
      reliable stream that is connected to an endpoint. *)

  val read: flow -> (Cstruct.t or_eof, error) result Lwt.t
  (** [read flow] blocks until some data is available and returns a
      fresh buffer containing it.

      The returned buffer will be of a size convenient to the flow
      implementation, but will always have at least 1 byte.

      If the remote endpoint calls [close] then calls to [read] will
      keep returning data until all the in-flight data has been read.
      [read flow] will return [`Eof] when the remote endpoint has
      called [close] and when there is no more in-flight data.
   *)

  val write: flow -> Cstruct.t -> (unit, write_error) result Lwt.t
  (** [write flow buffer] writes a buffer to the flow. There is no
      indication when the buffer has actually been read and, therefore,
      it must not be reused.  The contents may be transmitted in
      separate packets, depending on the underlying transport. The
      result [Ok ()] indicates success, [Error `Closed] indicates that the
      connection is now closed and therefore the data could not be
      written.  Other errors are possible. *)

  val writev: flow -> Cstruct.t list -> (unit, write_error) result Lwt.t
  (** [writev flow buffers] writes a sequence of buffers to the flow.
      There is no indication when the buffers have actually been read and,
      therefore, they must not be reused. The
      result [Ok ()] indicates success, [Error `Closed] indicates that the
      connection is now closed and therefore the data could not be
      written.  Other errors are possible. *)

  val close: flow -> unit Lwt.t
  (** [close flow] flushes all pending writes and signals the remote
      endpoint that there will be no future writes. Once the remote endpoint
      has read all pending data, it is expected that calls to [read] on
      the remote return [`Eof].

      Note it is still possible for the remote endpoint to [write] to
      the flow and for the local endpoint to call [read]. This state where
      the local endpoint has called [close] but the remote endpoint
      has not called [close] is similar to that of a half-closed TCP
      connection or a Unix socket after [shutdown(SHUTDOWN_WRITE)].

      [close flow] waits until the remote endpoint has also called [close]
      before returning. At this point no data can flow in either direction
      and resources associated with the flow can be freed.
      *)
end

(** {1 Copy stats} *)

type stats = {
  read_bytes: int64;
  read_ops: int64;
  write_bytes: int64;
  write_ops: int64;
  duration: int64;
}
(** The type for I/O statistics from a copy operation. *)

val pp_stats: stats Fmt.t
(** [pp_stats] is the pretty-printer for flow stats. *)

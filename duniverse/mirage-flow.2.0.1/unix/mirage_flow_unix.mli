(*
 * Copyright (c) 2015-present Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Conversion from mirage flows to Lwt_io channels. *)

module Make (F: Mirage_flow.S): sig

  val ic: ?buffer_size:int -> ?close:bool -> F.flow -> Lwt_io.input_channel
  (** Build an [Lwt_io] input channel from a mirage flow. If [close]
      is omitted, the mirage flow will be closed when the input
      channel is closed. *)

  val oc: ?buffer_size:int -> ?close:bool -> F.flow -> Lwt_io.output_channel
  (** Build an [Lwt_io] output channel from a mirage flow. If [close]
      is omitted, the mirage flow will {e not} be closed when the
      output channel is closed. *)

end

module Fd: Mirage_flow.S with type flow = Lwt_unix.file_descr

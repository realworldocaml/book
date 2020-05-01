(*{{{ Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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
 *
  }}}*)

(** Basic satisfaction of {! Cohttp_lwt.Net } *)

module IO = Io

type ctx = {
  ctx : Conduit_lwt_unix.ctx;
  resolver : Resolver_lwt.t;
} [@@deriving sexp_of]

val init : ?ctx:Conduit_lwt_unix.ctx -> ?resolver:Resolver_lwt.t -> unit -> ctx

val default_ctx : ctx

val connect_uri :
  ctx:ctx ->
  Uri.t ->
  (Conduit_lwt_unix.flow * Conduit_lwt_unix.ic * Conduit_lwt_unix.oc) Lwt.t

val close_in : 'a Lwt_io.channel -> unit
val close_out : 'a Lwt_io.channel -> unit

val close  : 'a Lwt_io.channel -> 'b Lwt_io.channel -> unit

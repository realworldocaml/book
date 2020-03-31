(*
 * Copyright (c) 2014 Hannes Mehnert <hannes@mehnert.org>
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
 *)

(** TLS/SSL connections via OCaml-TLS *)

module Client : sig

  val connect :
    ?src:Lwt_unix.sockaddr ->
    string ->
    Lwt_unix.sockaddr ->
    (Lwt_unix.file_descr * Lwt_io.input_channel * Lwt_io.output_channel) Lwt.t

end

module Server : sig
  val init
    : ?backlog:int
    -> certfile:string
    -> keyfile:string
    -> ?stop:(unit Lwt.t)
    -> ?timeout:int
    -> Lwt_unix.sockaddr
    -> (Lwt_unix.sockaddr
        -> Lwt_unix.file_descr
        -> Lwt_io.input_channel
        -> Lwt_io.output_channel
        -> unit Lwt.t)
    -> unit Lwt.t

  val init'
    : ?backlog:int
    -> ?stop:(unit Lwt.t)
    -> ?timeout:int
    -> 'config
    -> Lwt_unix.sockaddr
    -> (Lwt_unix.sockaddr
        -> Lwt_unix.file_descr
        -> Lwt_io.input_channel
        -> Lwt_io.output_channel
        -> unit Lwt.t)
    -> unit Lwt.t
end

(**/**)

val available : bool

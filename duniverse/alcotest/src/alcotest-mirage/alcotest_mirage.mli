(*
 * Copyright (c) 2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** [Alcotest_mirage] enables testing functions which return an Lwt promise.
    {!run} returns a promise that runs the tests when scheduled, catching any
    asynchronous exceptions thrown by the tests.

    Please note that this backend does not support redirection of standard
    streams into files (MirageOS does not have a file system). It writes all
    test output to the console. *)

module Make (C : Mirage_clock.MCLOCK) : sig
  include Alcotest_engine.Cli.S with type return = unit Lwt.t

  val test_case :
    string -> speed_level -> (Lwt_switch.t -> 'a -> unit Lwt.t) -> 'a test_case

  val test_case_sync : string -> speed_level -> ('a -> unit) -> 'a test_case
end

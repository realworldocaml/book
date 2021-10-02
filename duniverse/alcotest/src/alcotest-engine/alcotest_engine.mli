(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** [Alcotest_engine] provides a platform-independent test framework.

    The main building blocks and combinators are defined here. These can be used
    to defined tests. The platform-specific runners for these tests are in
    [alcotest], [alcotest-lwt], [alcotest-async] and [alcotest-mirage]. *)

(** {1 Assert functions} *)

module Test = Test

(** {1 Monadic test runners} *)

(** These modules provide the ability to run tests inside a concurrency monad:
    that is, to sequence test cases of type ['a -> unit m] into a computation of
    type ['a -> unit m] (for some concurrency monad [m]) with can then be
    scheduled in a main event loop. For tests using [Lwt.t] or
    [Async_kernel.Deferred.t], use the [Alcotest_lwt] and [Alcotest_async]
    packages directly. *)

module Core = Core
(** Defines monadic test runners {i without} command-line interfaces. *)

module Cli = Cli
(** Wraps {!Core} to provide a command-line interface. *)

module Monad = Monad
(** Monad signatures for use with {!Core} and {!Cli}. *)

module Platform = Platform
(** Defines platform-dependent functions. *)

(** These modules are exposed for use internally by other Alcotest packages.
    They do not provide a stable interface. *)
module Private : sig
  module Utils = Utils
  module Pp = Pp
end

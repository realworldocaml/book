(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** A lightweight and colourful test framework.

    [Alcotest] provides a simple interface to perform unit tests. It exposes a
    simple {{!TESTABLE} TESTABLE} module type, a {{!check} check function} to
    assert test predicates and a {{!run} run} function to perform a list of
    [unit -> unit] test callbacks.

    From these descriptions, [Alcotest] builds a quiet and colorful output where
    only faulty runs are fully displayed at the end of the run (with the full
    logs ready to inspect).

    {e Release 1.4.0} *)

include Alcotest_engine.Cli.S with type return = unit

include module type of Alcotest_engine.Test
(** @inline *)

(** {1 Unix-specific engine constructors}

    The [Alcotest_engine] package provides the most general form of the Alcotest
    API, parameterised over the thread implementation and the platform. This
    package provides the [Unix] platform implementation. *)

open Alcotest_engine
module Unix : Platform.MAKER

(** {!Core.Make} is [Alcotest_engine.Core.Make (Unix)] *)
module Core : sig
  module Make : module type of Alcotest_engine.Core.Make (Unix)
end

(** {!Cli.Make} is [Alcotest_engine.Cli.Make (Unix)] *)
module Cli : sig
  module Make : module type of Alcotest_engine.Cli.Make (Unix)
end

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

(** {1 Testable values}

    The following combinators represent types that can be used with the {!check}
    functions below. *)

(** [TESTABLE] provides an abstract description for testable values. *)
module type TESTABLE = sig
  type t
  (** The type to test. *)

  val pp : t Fmt.t
  (** A way to pretty-print the value. *)

  val equal : t -> t -> bool
  (** Test for equality between two values. *)
end

type 'a testable = (module TESTABLE with type t = 'a)
(** The type for testable values. *)

val testable : 'a Fmt.t -> ('a -> 'a -> bool) -> 'a testable
(** [testable pp eq] is a new {!type-testable} with the pretty-printer [pp] and
    equality [eq]. *)

val pp : 'a testable -> 'a Fmt.t
(** [pp t] is [t]'s pretty-printer. *)

val equal : 'a testable -> 'a -> 'a -> bool
(** [equal t] is [t]'s equality. *)

val bool : bool testable
(** [bool] tests booleans. *)

val int : int testable
(** [int] tests integers. *)

val int32 : int32 testable
(** [int32] tests 32-bit integers. *)

val int64 : int64 testable
(** [int64] tests 64-bit integers. *)

val float : float -> float testable
(** [float] tests floats with specified absolute error. *)

val char : char testable
(** [char] tests characters. *)

val string : string testable
(** [string] tests OCaml strings. *)

val bytes : bytes testable
(** [bytes] tests OCaml bytes. *)

val unit : unit testable
(** [unit] tests unit values (useful for functions with side-effects). *)

val list : 'a testable -> 'a list testable
(** [list t] tests lists of [t]s. *)

val slist : 'a testable -> ('a -> 'a -> int) -> 'a list testable
(** [slist t comp] tests sorted lists of [t]s. The list are sorted using [comp]. *)

val array : 'a testable -> 'a array testable
(** [array t] tests arrays of [t]s. *)

val option : 'a testable -> 'a option testable
(** [option t] tests optional [t]s. *)

val result : 'a testable -> 'e testable -> ('a, 'e) result testable
(** [result t e] tests [t]s on success and [e]s on failure. *)

val pair : 'a testable -> 'b testable -> ('a * 'b) testable
(** [pair a b] tests pairs of [a]s and [b]s. *)

val triple :
  'a testable -> 'b testable -> 'c testable -> ('a * 'b * 'c) testable
(** [triple a b c] tests triples of [a]s, [b]s and [c]s. *)

val of_pp : 'a Fmt.t -> 'a testable
(** [of_pp pp] tests values which can be printed using [pp] and compared using
    {!Stdlib.compare} *)

val pass : 'a testable
(** [pass] tests values of any type and always succeeds. *)

val reject : 'a testable
(** [reject] tests values of any type and always fails. *)

val neg : 'a testable -> 'a testable
(** [neg t] is [t]'s negation: it is [true] when [t] is [false] and it is
    [false] when [t] is [true]. *)

(** {1 Assertion functions}

    Functions for asserting various properties within unit-tests. A failing
    assertion will cause the testcase to fail immediately. *)

module Source_code_position : sig
  type here = Lexing.position
  (** Location information passed via a [~here] argument, intended for use with
      a PPX such as {{:https://github.com/janestreet/ppx_here} [ppx_here]}. *)

  type pos = string * int * int * int
  (** Location information passed via a [~pos] argument, intended for use with
      the [__POS__] macro provided by the standard library. See the
      documentation of [__POS__] for more information. *)
end

type 'a extra_info =
  ?here:Source_code_position.here -> ?pos:Source_code_position.pos -> 'a
(** The assertion functions optionally take information about the {i location}
    at which they are called in the source code. This is used for giving more
    descriptive error messages in the case of failure. *)

val check : ('a testable -> string -> 'a -> 'a -> unit) extra_info
(** Check that two values are equal. *)

val check' :
  ('a testable -> msg:string -> expected:'a -> actual:'a -> unit) extra_info
(** Check that two values are equal (labeled variant of {!check}). *)

val fail : (string -> 'a) extra_info
(** Simply fail. *)

val failf : (('a, Format.formatter, unit, 'b) format4 -> 'a) extra_info
(** Simply fail with a formatted message. *)

val check_raises : (string -> exn -> (unit -> unit) -> unit) extra_info
(** Check that an exception is raised. *)

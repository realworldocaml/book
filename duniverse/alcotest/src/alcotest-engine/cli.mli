(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2019 Craig Ferguson <me@craigfe.io>
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

(** This module extends {!Core} to allow CLI options to be passed to Alcotest
    executables. In particular:

    - tests can be selectively executed using the "test" subcommand;
    - the {!run_with_args} function can be used to pass arguments to tests from
      the command line;
    - all of the regular options to Alcotest.run can be set via CLI flags. *)

module type S = sig
  include Core.S
  (** @inline *)

  val run :
    (?argv:string array -> string -> unit test list -> return) with_options
  (** [run n t] runs the test suite [t]. [n] is the name of the tested library.

      The optional argument [and_exit] controls what happens when the function
      ends. By default, [and_exit] is set, which makes the function exit with
      [0] if everything is fine or [1] if there is an issue. If [and_exit] is
      [false], then the function raises [Test_error] on error.

      The optional argument [argv] specifies command line arguments sent to
      alcotest like ["--json"], ["--verbose"], etc. Note that this array will be
      treated like a regular [Sys.argv], so the array must have at least one
      element, and the first element will be treated as if it was the command
      name and thus ignored for the purposes of option processing. So
      [~argv:\[||\]] is an error, [~argv:\[| "--verbose" |\]] will have no
      effect, and [~argv:\[| "ignored"; "--verbose" |\]] will successfully pass
      the verbose option. *)

  val run_with_args :
    (?argv:string array ->
    string ->
    'a Cmdliner.Term.t ->
    'a test list ->
    return)
    with_options
  (** [run_with_args n a t] Similar to [run a t] but take an extra argument [a].
      Every test function will receive as argument the evaluation of the
      [Cmdliner] term [a]: this is useful to configure the test behaviors using
      the CLI. *)
end

module type MAKER = functor (P : Platform.MAKER) (M : Monad.S) ->
  S with type return = unit M.t

module Make (P : Platform.MAKER) (M : Monad.S) : S with type return = unit M.t

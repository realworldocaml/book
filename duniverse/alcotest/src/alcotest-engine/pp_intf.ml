(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2019      Craig Ferguson    <craig@tarides.com>
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

open Model

type theta = Format.formatter -> unit
(** Type corresponding to a [%t] placeholder. *)

module Types = struct
  type event = [ `Result of Test_name.t * Run_result.t | `Start of Test_name.t ]

  type result = {
    success : int;
    failures : int;
    time : float;
    errors : unit Fmt.t list;
  }
end

module type Make_arg = sig
  val stdout_columns : unit -> int option
end

module type S = sig
  include module type of Types

  val info :
    ?available_width:int ->
    max_label:int ->
    doc_of_test_name:(Test_name.t -> string) ->
    Test_name.t Fmt.t

  val rresult_error : Run_result.t Fmt.t

  val event_line :
    margins:int ->
    max_label:int ->
    doc_of_test_name:(Test_name.t -> string) ->
    [ `Result of Test_name.t * [< Run_result.t ] | `Start of Test_name.t ] Fmt.t

  val event :
    isatty:bool ->
    compact:bool ->
    max_label:int ->
    doc_of_test_name:(Test_name.t -> string) ->
    selector_on_failure:bool ->
    tests_so_far:int ->
    event Fmt.t

  val suite_results :
    log_dir:theta ->
    < verbose : bool ; show_errors : bool ; json : bool ; compact : bool ; .. > ->
    result Fmt.t

  val quoted : 'a Fmt.t -> 'a Fmt.t
  (** Wraps a formatter with `GNU-style quotation marks'. *)

  val with_surrounding_box : 'a Fmt.t -> 'a Fmt.t
  (** Wraps a formatter with a Unicode box with width given by
      {!X.stdout_columns}. Uses box-drawing characters from code page 437. *)

  val horizontal_rule : _ Fmt.t
  (** Horizontal rule of length {!X.stdout_columns}. Uses box-drawing characters
      from code page 437. *)

  val user_error : string -> _
  (** Raise a user error, then fail. *)
end

module type Pp = sig
  val tag : [ `Ok | `Fail | `Skip | `Todo | `Assert ] Fmt.t
  val map_theta : theta -> f:(unit Fmt.t -> unit Fmt.t) -> theta

  val pp_plural : int Fmt.t
  (** This is for adding an 's' to words that should be pluralized, e.g.

      {[
        let n = List.length items in
        Fmt.pr "Found %i item%a." n pp_plural n
      ]} *)

  module Make (X : Make_arg) : S
end

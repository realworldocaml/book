(*
 * Copyright (c) 2019 Nathan Rebours <nathan.p.rebours@gmail.com>
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

open Result

module Result : sig
  module Infix : sig
    val (>>=) : ('a, 'err) result -> ('a -> ('b, 'err) result) -> ('b, 'err) result

    val (>>|) : ('a, 'err) result -> ('a -> 'b) -> ('b, 'err) result
  end

  module List : sig
    val fold :
      f: ('acc -> 'a -> ('acc, 'err) result) ->
      init: 'acc ->
      'a list ->
      ('acc, 'err) result

    val map :
      f: ('a -> ('b, 'err) result) ->
      'a list ->
      ('b list, 'err) result
  end
end

module File : sig
  val read_lines : string -> string list
end

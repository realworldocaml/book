(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Misc
open Compat

type t = [ `Output of string | `Ellipsis ]

let dump ppf = function
  | `Output s -> Fmt.pf ppf "`Output %S" s
  | `Ellipsis -> Fmt.pf ppf "`Ellipsis"

let pp ?(pad = 0) ppf = function
  | `Output s -> Fmt.pf ppf "%a%s\n" pp_pad pad s
  | `Ellipsis -> Fmt.pf ppf "%a...\n" pp_pad pad

let equals_sub l r start length =
  let stop = start + length in
  let rec loop i = i = stop || (l.[i] = r.[i] && loop (succ i)) in
  loop start

let ellipsis_equal = function
  | `Output l, `Output r ->
      let length_r = String.length r in
      let length_l = String.length l in
      length_r > 3
      && r.[length_r - 3] = '.'
      && r.[length_r - 2] = '.'
      && r.[length_r - 1] = '.'
      && length_l > length_r - 4
      && equals_sub l r 0 (length_r - 3)
  | _, _ -> false

let equal a b =
  let rec aux x y =
    match (x, y) with
    | [], [] | [ `Ellipsis ], _ | _, [ `Ellipsis ] -> true
    | (`Ellipsis :: a as x), (_ :: b as y) | (_ :: b as y), (`Ellipsis :: a as x)
      ->
        aux x b
        || (* n+ matches: skip y's head *)
        aux a y
        (* 0  match  : skip x's head *)
    | a :: b, h :: t -> (a = h || ellipsis_equal (a, h)) && aux b t
    | _ -> false
  in
  aux a b

let drop_until xs x =
  let rec loop = function
    | `Output v :: xs when not (String.equal v x) -> loop xs
    | xs -> xs
  in
  loop xs

let merge (a : [ `Output of string ] list) (b : t list) =
  let rec aux (acc : t list) in_sync = function
    | a, [] ->
        List.rev_append acc
          (a : [ `Output of string ] list :> [> `Output of string ] list)
    | a, ([ `Ellipsis ] as b) ->
        List.rev_append acc
          ((a : [ `Output of string ] list :> [> `Output of string ] list) @ b)
    | [], _ -> List.rev acc
    | `Output l :: xs, `Output r :: ys ->
        aux (`Output l :: acc) (String.equal l r) (xs, ys)
    | xs, `Ellipsis :: (`Ellipsis :: _ as ys) -> aux acc in_sync (xs, ys)
    | xs, `Ellipsis :: (`Output y :: _ as ys) ->
        if in_sync then
          let rest = drop_until xs y in
          if List.compare_length_with rest 0 = 0 then aux acc in_sync (xs, ys)
          else aux (`Ellipsis :: acc) in_sync (rest, ys)
        else aux acc in_sync (xs, ys)
  in
  aux [] true (a, b)

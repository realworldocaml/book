(*{{{ Copyright (c) 2014 Rudi Grinberg
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

open Sexplib0.Sexp_conv

type t = [
  | `Empty
  | `String of string
  | `Strings of string list
] [@@deriving sexp]

let empty = `Empty

let is_empty = function
  | `Empty
  | `String ""
  | `Strings [] -> true
  | `String _
  | `Strings _ -> false

let to_string = function
  | `Empty -> ""
  | `String s -> s
  | `Strings sl -> String.concat "" sl

let to_string_list = function
  | `Empty -> []
  | `String s -> [s]
  | `Strings sl -> sl

let of_string s = `String s
let of_string_list s = `Strings s

let transfer_encoding = function
  | `Empty -> Transfer.Fixed 0L
  | `String s -> Transfer.Fixed (Int64.of_int (String.length s))
  | `Strings _ -> Transfer.Chunked

let length = function
  | `Empty -> 0L
  | `String s -> Int64.of_int (String.length s)
  | `Strings sl ->
    sl
    |> List.fold_left (fun a b ->
        b |> String.length |> Int64.of_int |> Int64.add a) 0L

let map f = function
  | `Empty -> `Empty
  | `String s -> `String (f s)
  | `Strings sl -> `Strings (List.map f sl)

(* TODO: maybe add a functor here that uses IO.S *)

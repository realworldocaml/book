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

open Compat
open Result

module Result = struct
  module Infix = struct
    let ( >>= ) r f = match r with Ok x -> f x | Error _ as e -> e

    let ( >>| ) r f = match r with Ok x -> Ok (f x) | Error _ as e -> e

    let ( >>! ) r f =
      match r with
      | Ok x -> f x
      | Error (`Msg e) ->
          Printf.eprintf "[mdx] Fatal error: %s\n" e;
          1
  end

  let errorf fmt = Format.ksprintf (fun s -> Error (`Msg s)) fmt

  module List = struct
    open Infix

    let fold ~f ~init l =
      let rec go acc = function
        | [] -> Ok acc
        | hd :: tl -> f acc hd >>= fun acc -> go acc tl
      in
      go init l

    let map ~f l =
      fold ~f:(fun acc elm -> f elm >>| fun elm' -> elm' :: acc) ~init:[] l
      >>| List.rev
  end
end

module File = struct
  let read_lines file =
    let ic = open_in file in
    let r = ref [] in
    try
      while true do
        r := input_line ic :: !r
      done;
      assert false
    with End_of_file ->
      close_in ic;
      List.rev !r
end

module Option = struct
  let is_some = function Some _ -> true | None -> false

  let value ~default = function Some v -> v | None -> default
end

module Sexp = struct
  type t = Atom of string | List of t list
end

module Csexp = Csexp.Make (Sexp)

module String = struct
  let english_concat ~last_sep words =
    let pf = Printf.sprintf in
    let rec aux acc = function
      | [] -> acc
      | [ last ] -> pf "%s %s %s" acc last_sep last
      | hd :: tl -> aux (pf "%s, %s" acc hd) tl
    in
    match words with
    | [] -> invalid_arg "Util.String.english_concat"
    | hd :: tl -> aux hd tl

  let english_conjonction words = english_concat ~last_sep:"and" words
end

module List = struct
  let find_map f l =
    let rec aux = function
      | [] -> None
      | h :: t -> ( match f h with Some x -> Some x | None -> aux t )
    in
    aux l
end

module Array = struct
  let slice t ~from ~to_ =
    let start_index, length = (from, to_ - from + 1) in
    Array.sub t start_index length
end

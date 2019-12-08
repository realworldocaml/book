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

module Result = struct
  module Infix = struct
    let (>>=) r f =
      match r with
      | Ok x -> f x
      | Error _ as e -> e

    let (>>|) r f =
      match r with
      | Ok x -> Ok (f x)
      | Error _ as e -> e
  end

  module List = struct
    open Infix

    let fold ~f ~init l =
      let rec go acc = function
        | [] -> Ok acc
        | hd::tl ->
          f acc hd >>= fun acc ->
          go acc tl
      in
      go init l

    let map ~f l =
      fold ~f:(fun acc elm -> f elm >>| fun elm' -> elm'::acc) ~init:[] l >>| List.rev
  end
end

module File = struct
  let read_lines file =
    let ic = open_in file in
    let r = ref [] in
    try while true do r := input_line ic :: !r done; assert false
    with End_of_file ->
      close_in ic;
      List.rev !r
end

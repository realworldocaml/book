(*
 * Copyright (c) 2009 Anil Madhavapeddy <anil@recoil.org>
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

(* Retrieve file extension, if any, or blank string otherwise *)
let get_extension filename =
  let rec search_dot i =
    if i < 1 || filename.[i] = '/' then ""
    else if filename.[i] = '.' then
      String.sub filename (i+1) (String.length filename - i - 1)
    else search_dot (i - 1) in
  search_dot (String.length filename - 1)

(* Given a full filename, lookup its MIME type *)
let lookup ?default filename =
  match get_extension filename with
  | "" -> Mime_types.map_file ?default filename
  | ext -> Mime_types.map_extension ?default (String.lowercase_ascii ext)

let reverse_lookup mime =
  let mime' =
    let string_length = String.length mime in
    let rec strip_parameters i =
      if i = string_length || mime.[i] = ';' then
        String.sub mime 0 i
      else
        strip_parameters (i + 1)
    in
    strip_parameters 0
  in
  Mime_types.map_mime mime'

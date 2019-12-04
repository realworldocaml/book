(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Commonalities for strings and substrings *)

open Astring_unsafe

let strf = Format.asprintf

(* Errors *)

let err_empty_string = "the string is empty"
let err_empty_sep = "~sep is an empty string"
let err_neg_max max = strf "negative ~max (%d)" max
let err_neg_min max = strf "negative ~min (%d)" max
let err_neg_len len = strf "negative length (%d)" len
let err_max_string_len = "Sys.max_string_length exceeded"

(* Base *)

let empty = ""

(* Predicates *)

let for_all sat s ~first ~last =
  let rec loop i =
    if i > last then true else
    if sat (string_unsafe_get s i) then loop (i + 1) else false
  in
  loop first

let exists sat s ~first ~last =
  let rec loop i =
    if i > last then false else
    if sat (string_unsafe_get s i) then true else loop (i + 1)
  in
  loop first

(* Traversing *)

let fold_left f acc s ~first ~last =
  let rec loop acc i =
    if i > last then acc else
    loop (f acc (string_unsafe_get s i)) (i + 1)
  in
  loop acc first

let fold_right f s acc ~first ~last =
  let rec loop i acc =
    if i < first then acc else
    loop (i - 1) (f (string_unsafe_get s i) acc)
  in
  loop last acc

(* OCaml conversions *)

let of_char c =
  let b = Bytes.create 1 in
  bytes_unsafe_set b 0 c;
  bytes_unsafe_to_string b

let to_char s = match string_length s with
| 0 -> None
| 1 -> Some (string_unsafe_get s 0)
| _ -> None

let of_bool = string_of_bool
let to_bool s =
  try Some (bool_of_string s) with Invalid_argument (* good joke *) _ -> None

let of_int = string_of_int
let to_int s = try Some (int_of_string s) with Failure _ -> None
let of_nativeint = Nativeint.to_string
let to_nativeint s = try Some (Nativeint.of_string s) with Failure _ -> None
let of_int32 = Int32.to_string
let to_int32 s = try Some (Int32.of_string s) with Failure _ -> None
let of_int64 = Int64.to_string
let to_int64 s = try Some (Int64.of_string s) with Failure _ -> None
let of_float = Pervasives.string_of_float
let to_float s = try Some (float_of_string s) with Failure _ -> None

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

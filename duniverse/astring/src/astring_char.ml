(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let err_byte b = Printf.sprintf "%d is not a byte" b

(* Bytes *)

type t = char

let unsafe_of_byte = Astring_unsafe.char_unsafe_of_byte

let of_byte b =
  if b < 0 || b > 255 then invalid_arg (err_byte b) else unsafe_of_byte b

let of_int b =
  if b < 0 || b > 255 then None else (Some (unsafe_of_byte b))

let to_int = Astring_unsafe.char_to_byte

let hash c = Hashtbl.hash c

(* Predicates *)

let equal : t -> t -> bool = fun c0 c1 -> c0 = c1
let compare : t -> t -> int = fun c0 c1 -> Pervasives.compare c0 c1

(* Bytes as US-ASCII characters *)

module Ascii = struct
  let max_ascii = '\x7F'

  let is_valid : t -> bool = fun c -> c <= max_ascii

  let is_digit = function '0' .. '9' -> true | _ -> false

  let is_hex_digit = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
  | _ -> false

  let is_upper = function 'A' .. 'Z' -> true | _ -> false

  let is_lower = function 'a' .. 'z' -> true | _ -> false

  let is_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

  let is_alphanum = function
  | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false

  let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false

  let is_blank = function ' ' | '\t' -> true | _ -> false

  let is_graphic = function '!' .. '~' -> true | _ -> false

  let is_print = function ' ' .. '~' -> true | _ -> false

  let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

  let uppercase = function
  | 'a' .. 'z' as c -> unsafe_of_byte @@ to_int c - 0x20
  | c -> c

  let lowercase = function
  | 'A' .. 'Z' as c -> unsafe_of_byte @@ to_int c + 0x20
  | c -> c

  (* Escaping *)

  let escape = Astring_escape.char_escape
  let escape_char = Astring_escape.char_escape_char
end

(* Pretty printing *)

let pp = Format.pp_print_char
let dump ppf c =
  Format.pp_print_char ppf '\'';
  Format.pp_print_string ppf (Ascii.escape_char c);
  Format.pp_print_char ppf '\'';
  ()

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

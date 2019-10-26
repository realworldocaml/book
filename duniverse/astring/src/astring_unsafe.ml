(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Unsafe string and byte manipulations. If you don't believe the
   author's invariants, replacing with safe versions makes everything
   safe in the library. He won't be upset. *)

let array_unsafe_get = Array.unsafe_get

external char_unsafe_of_byte : int -> char = "%identity"
external char_to_byte : char -> int = "%identity"

let bytes_unsafe_set = Bytes.unsafe_set
let bytes_unsafe_to_string = Bytes.unsafe_to_string
let bytes_unsafe_blit_string s sfirst d dfirst len =
  Bytes.(unsafe_blit (unsafe_of_string s) sfirst d dfirst len)

external string_length : string -> int = "%string_length"
external string_equal : string -> string -> bool = "caml_string_equal"
external string_compare : string -> string -> int = "caml_string_compare"
external string_safe_get : string -> int -> char = "%string_safe_get"
external string_unsafe_get : string -> int -> char = "%string_unsafe_get"

let unsafe_string_sub s first len =
  let b = Bytes.create len in
  bytes_unsafe_blit_string s first b 0 len;
  Bytes.unsafe_to_string b

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

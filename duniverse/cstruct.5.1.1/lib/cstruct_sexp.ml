(*
 * Copyright (c) 2012-2019 Anil Madhavapeddy <anil@recoil.org>
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

open Sexplib

type buffer = Cstruct.buffer
type t = Cstruct.t

let buffer_of_sexp b = Conv.bigstring_of_sexp b
let sexp_of_buffer b = Conv.sexp_of_bigstring b

let t_of_sexp = function
  | Sexp.Atom str ->
      let n = String.length str in
      let t = Cstruct.create_unsafe n in
      Cstruct.blit_from_string str 0 t 0 n ;
      t
  | sexp -> Conv.of_sexp_error "Cstruct.t_of_sexp: atom needed" sexp

let sexp_of_t t =
  let n   = Cstruct.len t in
  let str = Bytes.create n in
  Cstruct.blit_to_bytes t 0 str 0 n ;
  (* The following call is safe, since str is not visible elsewhere. *)
  Sexp.Atom (Bytes.unsafe_to_string str)

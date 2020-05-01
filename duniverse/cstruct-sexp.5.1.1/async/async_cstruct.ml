(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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

open Core_kernel
open Async_unix
open Async_kernel

let to_bigsubstring t =
  Bigsubstring.create 
    ~pos:t.Cstruct.off 
    ~len:t.Cstruct.len 
    t.Cstruct.buffer

let of_bigsubstring t =
  Cstruct.of_bigarray
    ~off:(Bigsubstring.pos t) 
    ~len:(Bigsubstring.length t)
    (Bigsubstring.base t)

let read rd t =
  Reader.read_bigsubstring rd (to_bigsubstring t)

let schedule_write wr t =
  let open Cstruct in
  Writer.schedule_bigstring ~pos:t.off ~len:t.len wr t.buffer

module Pipe = struct
  let map_string rd wr =
    let rd = Pipe.map rd ~f:Cstruct.to_string in
    let rd',wr' = Pipe.create () in
    don't_wait_for (Pipe.transfer rd' wr ~f:Cstruct.of_string);
    rd,wr'

  let map_bigsubstring rd wr =
    let rd = Pipe.map rd ~f:to_bigsubstring in
    let rd',wr' = Pipe.create () in
    don't_wait_for (Pipe.transfer rd' wr ~f:of_bigsubstring);
    rd,wr'
end

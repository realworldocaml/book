(*
 * Copyright (c) 2012-2019 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2019 Romain Calascibetta <romain.calascibetta@gmail.com>
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

include (Cstruct : module type of Cstruct with type t := Cstruct.t)

type 'a rd = < rd: unit; .. > as 'a
type 'a wr = < wr: unit; .. > as 'a

type 'a t = Cstruct.t

type rdwr =  < rd: unit; wr: unit; >
type ro = < rd: unit; >
type wo = < wr: unit; >

external ro : 'a rd t -> ro t = "%identity"
external wo : 'a wr t -> wo t = "%identity"

let of_string ?off ?len x =
  Cstruct.of_string ?off ?len x
let of_bytes ?off ?len x =
  Cstruct.of_bytes ?off ?len x

let to_string ?(off= 0) ?len t =
  let len = match len with
    | Some len -> len
    | None -> Cstruct.len t - off in
  Cstruct.copy t off len

let to_bytes ?(off= 0) ?len t =
  let len = match len with
    | Some len -> len
    | None -> Cstruct.len t - off in
  (* XXX(dinosaure): this is safe when [copy] allocates itself [bytes]
     and uses [Bytes.unsafe_to_string]. *)
  Bytes.unsafe_of_string (Cstruct.copy t off len)

let pp ppf t = Cstruct.hexdump_pp ppf t

let length = Cstruct.len

let blit src ~src_off dst ~dst_off ~len =
  Cstruct.blit src src_off dst dst_off len
[@@inline]

let blit_from_string src ~src_off dst ~dst_off ~len =
  Cstruct.blit_from_string src src_off dst dst_off len
[@@inline]

let blit_from_bytes src ~src_off dst ~dst_off ~len =
  Cstruct.blit_from_bytes src src_off dst dst_off len
[@@inline]

let blit_to_bytes src ~src_off dst ~dst_off ~len =
  Cstruct.blit_to_bytes src src_off dst dst_off len
[@@inline]

let sub t ~off ~len =
  Cstruct.sub t off len
[@@inline]

let concat vss =
  let res = create_unsafe (Cstruct.sum_lengths ~caller:"Cstruct.Cap.concat" vss) in
  let go off v =
    let len = Cstruct.len v in
    Cstruct.blit v 0 res off len ;
    off + len in
  let len = List.fold_left go 0 vss in
  assert (len = Cstruct.len res) ;
  res

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

val to_bigsubstring : Cstruct.t -> Bigsubstring.t
val of_bigsubstring : Bigsubstring.t -> Cstruct.t

val read: Reader.t -> Cstruct.t -> int Reader.Read_result.t Deferred.t
val schedule_write: Writer.t -> Cstruct.t -> unit

module Pipe : sig
  val map_string :
    Cstruct.t Pipe.Reader.t ->
    Cstruct.t Pipe.Writer.t ->
    (string Pipe.Reader.t * string Pipe.Writer.t)

  val map_bigsubstring :
    Cstruct.t Pipe.Reader.t ->
    Cstruct.t Pipe.Writer.t ->
    (Bigsubstring.t Pipe.Reader.t * Bigsubstring.t Pipe.Writer.t)
end

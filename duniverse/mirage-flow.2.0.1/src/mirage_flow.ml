(*
 * Copyright (C) 2016-present David Scott <dave.scott@docker.com>
 * Copyright (c) 2011-present Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-present Thomas Gazagnaire <thomas@gazagnaire.org>
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

type write_error = [ `Closed ]

let pp_write_error ppf = function
  | `Closed -> Fmt.pf ppf "attempted to write to a closed flow"

type 'a or_eof = [`Data of 'a | `Eof ]

let pp_or_eof d ppf = function
  | `Data a -> d ppf a
  | `Eof    -> Fmt.string ppf "End-of-file"

module type S = sig
  type error
  val pp_error: error Fmt.t
  type nonrec write_error = private [> write_error ]
  val pp_write_error: write_error Fmt.t
  type flow
  val read: flow -> (Cstruct.t or_eof, error) result Lwt.t
  val write: flow -> Cstruct.t -> (unit, write_error) result Lwt.t
  val writev: flow -> Cstruct.t list -> (unit, write_error) result Lwt.t
  val close: flow -> unit Lwt.t
end

type stats = {
  read_bytes: int64;
  read_ops: int64;
  write_bytes: int64;
  write_ops: int64;
  duration: int64;
}

let kib = 1024L
let ( ** ) = Int64.mul
let mib = kib ** 1024L
let gib = mib ** 1024L
let tib = gib ** 1024L

let suffix = [
  kib, "KiB";
  mib, "MiB";
  gib, "GiB";
  tib, "TiB";
]

let add_suffix x =
  List.fold_left (fun acc (y, label) ->
      if Int64.div x y > 0L
      then Printf.sprintf "%.1f %s" Int64.((to_float x) /. (to_float y)) label
      else acc
    ) (Printf.sprintf "%Ld bytes" x) suffix

let pp_stats ppf s =
  Fmt.pf ppf "%s bytes at %s/nanosec and %Lu IOPS/nanosec"
    (add_suffix s.read_bytes)
    (add_suffix Int64.(div s.read_bytes s.duration))
    (Int64.div s.read_ops s.duration)

(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
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

module Key = struct

  type t = string list
  (* Store the path as a reverse list to optimise basename and (/)
     operations *)

  let err_invalid_segment x = Fmt.failwith "%S is not a valid segment" x

  let check_segment x =
    String.iter (function '/' -> err_invalid_segment x | _ -> ()) x;
    x

  let empty = []
  let v s = List.filter ((<>)"") @@ List.rev (String.split_on_char '/' s)
  let add t v = (check_segment v) :: t
  let ( / ) = add
  let append x y = y @ x
  let ( // ) = append
  let segments = List.rev
  let basename = List.hd
  let parent = List.tl
  let compare = compare
  let equal = (=)
  let pp ppf l = Fmt.pf ppf "/%a" Fmt.(list ~sep:(unit "/") string) (List.rev l)
  let to_string = Fmt.to_to_string pp
end

type key = Key.t

type error = [
  | `Not_found           of key
  | `Dictionary_expected of key
  | `Value_expected      of key
]

let pp_error ppf = function
  | `Not_found k           -> Fmt.pf ppf "Cannot find the key %a" Key.pp k
  | `Dictionary_expected k ->
    Fmt.pf ppf "Expecting a dictionary for the key %a" Key.pp k
  | `Value_expected k      ->
    Fmt.pf ppf "Expecting a value for the key %a" Key.pp k

module type RO = sig
  type nonrec error = private [> error]
  val pp_error: error Fmt.t
  include Mirage_device.S
  type key = Key.t
  val exists: t -> key -> ([`Value | `Dictionary] option, error) result Lwt.t
  val get: t -> key -> (string, error) result Lwt.t
  val list: t -> key -> ((string * [`Value | `Dictionary]) list, error) result Lwt.t
  val last_modified: t -> key -> (int * int64, error) result Lwt.t
  val digest: t -> key -> (string, error) result Lwt.t
end

type write_error = [ error | `No_space | `Too_many_retries of int ]

let pp_write_error ppf = function
  | #error as e -> pp_error ppf e
  | `No_space   -> Fmt.pf ppf "No space left on device"
  | `Too_many_retries n ->
    Fmt.pf ppf "Aborting after %d attempts to apply the batch operations." n

module type RW = sig
  include RO
  type nonrec write_error = private [> write_error]
  val pp_write_error: write_error Fmt.t
  val set: t -> key -> string -> (unit, write_error) result Lwt.t
  val remove: t -> key -> (unit, write_error) result Lwt.t
  val batch: t -> ?retries:int -> (t -> 'a Lwt.t) -> 'a Lwt.t
end

open Or_error

(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
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

(** Utilities to manipulate files and paths. *)

type file

type directory

module Directory : sig

  open Or_error

  type t = directory

  val dirname : t -> t
  val basename : t -> t

  val append : t -> t -> t

  val reach_from : dir:t -> string -> t
  (** @raises [Invalid_arg _] if [parent/name] exists but is not a directory. *)

  val mkdir_p : t -> unit

  val of_string : string -> t
  val to_string : t -> string

  val fold_files_rec_result : ?ext:string ->
    ('a -> file -> ('a, msg) result) -> 'a -> t -> ('a, [> msg ]) result
  (** [fold_files_rec_result ~ext f acc d] recursively folds [f] over the files
      with extension matching [ext] (defaults to [""]) contained in [d]
      and its sub directories. Stop as soon as [f] returns [Error _]. *)

  module Table : Hashtbl.S with type key = t
end

module File : sig

  type t = file

  val create : directory:Directory.t -> name:string -> t

  val dirname : t -> Directory.t
  val basename : t -> t

  val set_ext : string -> t -> t
  val has_ext : string -> t -> bool

  val of_string : string -> t
  val to_string : t -> string

  val read : t -> (string, [> msg ]) result

  module Table : Hashtbl.S with type key = t
end

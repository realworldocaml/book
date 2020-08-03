(*
 * Copyright (c) 2020 Ulysse Gérard <ulysse@tarides.com>
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

open Compat

type t = File of string | Dir of string

let of_block block =
  let open Block in
  match (directory block, file block, skip block) with
  | Some d, Some f, false -> Some (File (Filename.concat d f))
  | Some d, None, false -> Some (Dir d)
  | None, Some f, false -> Some (File f)
  | None, None, false -> None
  | _, _, true -> None

let of_lines =
  let open Document in
  List.filter_map (function
    | Section _ | Text _ -> None
    | Block b -> of_block b)

let to_sexp t : Util.Sexp.t =
  match t with
  | Dir dir -> List [ Atom "dir"; Atom dir ]
  | File file -> List [ Atom "file"; Atom file ]

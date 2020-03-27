(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
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



let contains_double_underscore s =
  let len = String.length s in
  let rec aux i =
    if i > len - 2 then false else
    if s.[i] = '_' && s.[i + 1] = '_' then true
    else aux (i + 1)
  in
  aux 0

module Package =
struct
  type t = string

  module Table = Hashtbl.Make(struct
    type nonrec t = t
    let equal : t -> t -> bool = (=)
    let hash : t -> int = Hashtbl.hash
  end)
end

module Odoc_file =
struct
  type compilation_unit = {name : string; hidden : bool}

  type t =
    | Page of string
    | Compilation_unit of compilation_unit 

  let create_unit ~force_hidden name =
    let hidden = force_hidden || contains_double_underscore name in
    Compilation_unit {name; hidden}

  let create_page name = Page name

  let name = function
    | Page name
    | Compilation_unit {name; _} -> name
end

type t = {
  package : Package.t;
  file : Odoc_file.t;
  digest : Digest.t;
}

let equal : t -> t -> bool = (=)
let hash : t -> int = Hashtbl.hash

let to_string t = Printf.sprintf "%s::%s" t.package (Odoc_file.name t.file)

module Hash_table =
  Hashtbl.Make
    (struct
      type nonrec t = t
      let equal = equal
      let hash = hash
    end)

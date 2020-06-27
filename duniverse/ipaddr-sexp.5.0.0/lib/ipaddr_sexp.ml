(*
 * Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
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
 *
 *)

open Sexplib0

let of_sexp fn = function
  | Sexp.List _ -> failwith "expecting sexp atom"
  | Sexp.Atom s -> (
    match fn s with Ok r -> r | Error (`Msg msg) -> failwith msg )

let to_sexp fn t = Sexp.Atom (fn t)

module V4 = struct
  module I = Ipaddr.V4

  type t = I.t

  let sexp_of_t = to_sexp I.to_string

  let t_of_sexp = of_sexp I.of_string

  let compare = I.compare

  module Prefix = struct
    module I = Ipaddr.V4.Prefix

    type addr = I.addr

    type t = I.t

    let sexp_of_t = to_sexp I.to_string

    let t_of_sexp = of_sexp I.of_string

    let compare = I.compare
  end
end

module V6 = struct
  module I = Ipaddr.V6

  type t = I.t

  let sexp_of_t = to_sexp I.to_string

  let t_of_sexp = of_sexp I.of_string

  let compare = I.compare

  module Prefix = struct
    module I = Ipaddr.V6.Prefix

    type addr = I.addr

    type t = I.t

    let sexp_of_t = to_sexp I.to_string

    let t_of_sexp = of_sexp I.of_string

    let compare = I.compare
  end
end

module I = Ipaddr

type t = I.t

let sexp_of_t = to_sexp I.to_string

let t_of_sexp = of_sexp I.of_string

let compare = I.compare

type scope = I.scope

let sexp_of_scope = to_sexp I.string_of_scope

let scope_of_sexp = of_sexp I.scope_of_string

module Prefix = struct
  module I = Ipaddr.Prefix

  type addr = I.addr

  type t = I.t

  let sexp_of_t = to_sexp I.to_string

  let t_of_sexp = of_sexp I.of_string

  let compare = I.compare
end

(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

let to_inet_addr t =
  Unix.inet_addr_of_string (Ipaddr.to_string t)

let of_inet_addr t =
  Ipaddr.of_string_exn (Unix.string_of_inet_addr t)

module V4 = struct

  let to_inet_addr t =
    Unix.inet_addr_of_string (Ipaddr.V4.to_string t)

  let of_inet_addr_exn t =
    Ipaddr.V4.of_string_exn (Unix.string_of_inet_addr t)

  let of_inet_addr t =
    try Some (of_inet_addr_exn t)
    with _ -> None
end

module V6 = struct

  let to_inet_addr t =
    Unix.inet_addr_of_string (Ipaddr.V6.to_string t)

  let of_inet_addr_exn t =
    Ipaddr.V6.of_string_exn (Unix.string_of_inet_addr t)

  let of_inet_addr t =
    try Some (of_inet_addr_exn t)
    with _ -> None
end

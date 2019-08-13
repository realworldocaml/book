(*
 * Copyright (c) 2019 Anil Madhavapeddy
 * Copyright (c) 2014 Nicolás Ojeda Bär
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

let try_with_result fn a =
  try Ok (fn a)
  with Macaddr.Parse_error (msg, _) -> Error (`Msg ("Macaddr: " ^ msg))

let of_cstruct_exn cs =
  if Cstruct.len cs <> 6
  then raise (Macaddr.Parse_error ("MAC is exactly 6 bytes", Cstruct.to_string cs))
  else Cstruct.to_string cs |> Macaddr.of_octets_exn

let of_cstruct cs =
  try_with_result of_cstruct_exn cs

let write_cstruct_exn (mac:Macaddr.t) cs =
  let len = Cstruct.len cs in
  let mac = Macaddr.to_octets mac in
  if len <> 6 then raise (Macaddr.Parse_error ("MAC is exactly 6 bytes", mac));
  Cstruct.blit_from_string mac 0 cs 0 6

let to_cstruct ?(allocator = Cstruct.create) mac =
  let cs = allocator 6 in
  write_cstruct_exn mac cs;
  cs

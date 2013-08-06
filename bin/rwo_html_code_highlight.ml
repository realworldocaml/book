(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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

open Core.Std

let subst_with_code_frag _attr (c:Cow.Xml.t) : Cow.Xml.t =
  try
    let t = Code_frag.of_string (Cow.Html.to_string c) in
    In_channel.read_all (sprintf "code/_build/%s.%d.html" t.Code_frag.name t.Code_frag.part)
    |> Cow.Html.of_string
  with exn ->
    eprintf "WARNING bare <pre> found. This should be turned into a ```frag:\n %s\n%!" (Exn.to_string exn);
    Code_frag.wrap_in_pretty_box ~part:0 "Unknown" "" c

let () =
  try
    Xml_tree.read_document In_channel.stdin
    |> fun (dtd, doc) -> Xml_tree.map ~tag:"pre" ~f:subst_with_code_frag doc
    |> fun doc -> Xml_tree.write_document Out_channel.stdout dtd doc
  with 
  | Xmlm.Error (_p,e) ->
    print_endline (Xmlm.error_message e)

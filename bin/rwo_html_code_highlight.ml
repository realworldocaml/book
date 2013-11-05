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

let get_code_frag fname part =
  let fname = sprintf "%s/%s.%d.html" Sys.argv.(1) fname part in
  prerr_endline fname;
  Ezxmlm.from_channel (open_in fname)
  |> fun (dtd,xml) ->
  xml

let () =
  let open Ezxmlm in
  let dtd,xml = from_channel stdin in
  filter_map ~tag:"pre" ~f:(fun attrs nodes -> []) xml
  |> filter_map ~tag:"p" ~f:(fun attrs nodes ->
    (* Hunt for a <p> with a href to github *)
    try
       let attr,link = member_with_attr "a" nodes in
       let part = try
         Scanf.sscanf (String.lstrip (data_to_string nodes)) "(part %d)" (fun d -> d)
        with _ -> 0 in
       let fname = String.chop_prefix_exn ~prefix:"https://github.com/realworldocaml/examples/tree/v1/code/" (get_attr "href" attr) in
       let orig_node = [make_tag "p" (attrs, nodes)] in
       get_code_frag fname part
    with _ -> [make_tag "p" (attrs,nodes)]
  )
  |> fun d -> print_endline (Cow.Html.to_string d)

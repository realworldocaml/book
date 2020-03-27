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

open Or_error

let magic = "odoc-%%VERSION%%"

let load file ic =
  let m = really_input_string ic (String.length magic) in
  if m = magic then
    Ok (Marshal.from_channel ic)
  else
    let msg =
      Printf.sprintf "%s: invalid magic number %S, expected %S\n%!"
        file m magic
    in
    Error (`Msg msg)

let save oc t =
  output_string oc magic;
  Marshal.to_channel oc t []

let read file =
  let file = Fs.File.to_string file in
  let ic = open_in_bin file in
  let root = load file ic in
  close_in ic;
  root

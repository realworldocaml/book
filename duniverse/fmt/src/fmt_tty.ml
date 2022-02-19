(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fmt programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let is_infix ~affix s =
  (* Damned, already missing astring, from which this is c&p *)
  let len_a = String.length affix in
  let len_s = String.length s in
  if len_a > len_s then false else
  let max_idx_a = len_a - 1 in
  let max_idx_s = len_s - len_a in
  let rec loop i k =
    if i > max_idx_s then false else
    if k > max_idx_a then true else
    if k > 0 then
      if String.get affix k = String.get s (i + k) then loop i (k + 1) else
      loop (i + 1) 0
    else if String.get affix 0 = String.get s i then loop i 1 else
    loop (i + 1) 0
  in
  loop 0 0

let setup ?style_renderer ?utf_8 oc =
  let ppf =
    if oc == Stdlib.stdout then Fmt.stdout else
    if oc == Stdlib.stderr then Fmt.stderr else
    Format.formatter_of_out_channel oc
  in
  let style_renderer = match style_renderer with
  | Some r -> r
  | None ->
      let dumb =
        try match Sys.getenv "TERM" with
        | "dumb" | "" -> true
        | _ -> false
        with
        Not_found -> true
      in
      let isatty = try Unix.(isatty (descr_of_out_channel oc)) with
      | Unix.Unix_error _ -> false
      in
      if not dumb && isatty then `Ansi_tty else `None
  in
  let utf_8 = match utf_8 with
  | Some b -> b
  | None ->
      let has_utf_8 var =
        try is_infix "UTF-8" (String.uppercase_ascii (Sys.getenv var))
        with Not_found -> false
      in
      has_utf_8 "LANG" || has_utf_8 "LC_ALL" || has_utf_8 "LC_CTYPE"
  in
  Fmt.set_style_renderer ppf style_renderer;
  Fmt.set_utf_8 ppf utf_8;
  ppf

let setup_std_outputs ?style_renderer ?utf_8 () =
  ignore (setup ?style_renderer ?utf_8 stdout);
  ignore (setup ?style_renderer ?utf_8 stderr);
  ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The fmt programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

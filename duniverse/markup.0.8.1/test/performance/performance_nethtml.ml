(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Performance_common
open Nethtml

let (|>) x f = f x

let parse file =
  file
  |> open_in
  |> Lexing.from_channel
  |> parse_document ~dtd:relaxed_html40_dtd
  |> ignore

let () =
  measure 100 "nethtml" google_page "html" (fun () ->
    parse google_page);

  measure 100 "nethtml" xml_spec "html" (fun () ->
    parse xml_spec)

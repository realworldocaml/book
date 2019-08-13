(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Performance_common
open Markup

let (|>) x f = f x

let () =
  measure 100 "markup.ml" google_page "html" (fun () ->
    file google_page |> fst |> parse_html |> signals |> drain);

  measure 100 "markup.ml" xml_spec "xml" (fun () ->
    file xml_spec |> fst |> parse_xml |> signals |> drain)

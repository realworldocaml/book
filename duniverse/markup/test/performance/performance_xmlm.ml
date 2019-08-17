(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Performance_common
open Xmlm

let (|>) x f = f x

let parse file =
  try
    make_input ~entity:(fun _ -> Some "") (`Channel (open_in file))
    |> input_doc_tree ~el:(fun _ _ -> ()) ~data:ignore
    |> ignore
  with Xmlm.Error ((l, c), e) as exn ->
    Printf.printf "%i %i %s\n" l c (error_message e);
    raise exn

let () =
  measure 100 "xmlm" xml_spec "xml" (fun () ->
    parse xml_spec)

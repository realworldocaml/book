(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



module Common = Bisect_common

let conditional = ref false

let enabled () =
  match !conditional with
  | false ->
    `Enabled
  | true ->
    match Sys.getenv "BISECT_ENABLE" with
    | exception Not_found ->
      `Disabled
    | s when (String.uppercase [@ocaml.warning "-3"]) s = "YES" ->
      `Enabled
    | _ ->
      `Disabled

let conditional_exclude_file filename =
  match enabled () with
  | `Enabled -> Exclusions.add_from_file filename
  | `Disabled -> ()

let switches = [
  ("--exclude-files",
   Arg.String Exclusions.add_file,
   "<regexp>  Exclude files matching <regexp>");

  ("--exclusions",
   Arg.String conditional_exclude_file,
   "<filename>  Exclude functions listed in given file");

  ("--conditional",
  Arg.Set conditional,
  " Instrument only when BISECT_ENABLE is YES");

  ("--bisect-file",
  Arg.String (fun s -> Common.bisect_file := Some s),
  " Default value for BISECT_FILE environment variable");

  ("--bisect-silent",
  Arg.String (fun s -> Common.bisect_silent := Some s),
  " Default value for BISECT_SILENT environment variable");
]

let () =
  Arg.align switches
  |> List.iter (fun (key, spec, doc) -> Ppxlib.Driver.add_arg key spec ~doc)


let () =
  let impl ctxt ast =
    match enabled () with
    | `Enabled ->
      new Instrument.instrumenter#transform_impl_file ctxt ast
    | `Disabled ->
      ast
  in
  let instrument = Ppxlib.Driver.Instrument.V2.make impl ~position:After in
  Ppxlib.Driver.register_transformation ~instrument "bisect_ppx"

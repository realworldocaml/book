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
  ("--exclude",
   Arg.String (fun s ->
    prerr_endline "bisect_ppx argument '--exclude' is deprecated.";
    prerr_endline "Use '--exclusions' instead.";
    Exclusions.add s),
   " Deprecated");

  ("--exclude-files",
   Arg.String Exclusions.add_file,
   "<regexp>  Exclude files matching <regexp>");

  ("--exclusions",
   Arg.String conditional_exclude_file,
   "<filename>  Exclude functions listed in given file");

  ("--exclude-file",
   Arg.String (fun s ->
    prerr_endline "bisect_ppx argument '--exclude-file' is deprecated.";
    prerr_endline "It has been renamed to '--exclusions'.";
    conditional_exclude_file s),
   " Deprecated");

  ("--conditional",
  Arg.Set conditional,
  " Instrument only when BISECT_ENABLE is YES");

  ("--no-comment-parsing",
  Arg.Unit (fun () ->
    prerr_endline "bisect_ppx argument '--no-comment-parsing' is deprecated."),
  " Deprecated");

  ("-mode",
  (Arg.Symbol (["safe"; "fast"; "faster"], fun _ ->
    prerr_endline "bisect_ppx argument '-mode' is deprecated.")),
  " Deprecated") ;

  ("--bisect-file",
  Arg.String (fun s -> Common.bisect_file := Some s),
  " Default value for BISECT_FILE environment variable");

  ("--bisect-silent",
  Arg.String (fun s -> Common.bisect_silent := Some s),
  " Default value for BISECT_SILENT environment variable");
]

let deprecated = Common.deprecated "bisect_ppx"

let switches =
  switches
  |> deprecated "-exclude"
  |> deprecated "-exclude-file"
  |> deprecated "-conditional"
  |> deprecated "-no-comment-parsing"
  |> Arg.align



let () =
  Migrate_parsetree.Driver.register
    ~name:"bisect_ppx" ~args:switches ~position:100
    Migrate_parsetree.Versions.ocaml_411 begin fun _config _cookies ->
      match enabled () with
      | `Enabled ->
        Ppx_tools_411.Ast_mapper_class.to_mapper (new Instrument.instrumenter)
      | `Disabled ->
        Migrate_parsetree.Ast_411.shallow_identity
    end

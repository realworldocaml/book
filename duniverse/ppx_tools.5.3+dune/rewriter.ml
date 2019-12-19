(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2014  Peter Zotov                                  *)

let inputs : ([ `Struct | `Sig ] * [ `String | `Path ] * string) list ref = ref []
let output_file : string ref = ref "-"
let tool_name = ref "ocamlc"

let args =
  let open Arg in
  align [
    "-ppx", String (fun s -> Clflags.all_ppx := s :: !Clflags.all_ppx),
    "<cmd> Invoke <cmd> as a ppx preprocessor";

    "-str", String (fun s -> inputs := (`Struct, `String, s) :: !inputs),
    "<str> Parse <str> as a structure";

    "-sig", String (fun s -> inputs := (`Sig, `String, s) :: !inputs),
    "<str> Parse <str> as a signature";

    "-impl", String (fun s -> inputs := (`Struct, `Path, s) :: !inputs),
    "<file> Parse <file> as an implementation (specify - for stdin)";

    "-intf", String (fun s -> inputs := (`Sig, `Path, s) :: !inputs),
    "<file> Parse <file> as an interface (specify - for stdin)";

    "-o", Set_string output_file,
    "<file> Write result into <file> (stdout by default)";

    "-tool-name", Set_string tool_name,
    "<str> Set tool name to <str> (ocamlc by default)";

    "-I", String (fun s -> Clflags.include_dirs := s :: !Clflags.include_dirs),
    "<dir> Add <dir> to the list of include directories";

    "-open", String (fun s -> Clflags.open_modules := s :: !Clflags.open_modules),
    "<module> Add <module> to the list of opened modules";

    "-for-pack", String (fun s -> Clflags.for_package := Some s),
    "<ident> Preprocess code as if it will be packed inside <ident>";

    "-g", Set Clflags.debug,
    " Request debug information from preprocessor";
  ]

let anon_arg s =
  match !Clflags.all_ppx with
  | [] -> Clflags.all_ppx := s :: !Clflags.all_ppx
  | _  -> inputs := (`Struct, `Path, s) :: !inputs

let usage_msg =
  Printf.sprintf
    "Usage: %s [ppx-rewriter] [options...] [implementations...]\n\
     If no implementations are specified, parses stdin."
    Sys.argv.(0)

let wrap_open fn file =
  try  fn file
  with Sys_error msg ->
    prerr_endline msg;
    exit 1

let make_lexer source_kind source =
  match source_kind, source with
  | `String, _ ->
      Location.input_name := "//toplevel//";
      Lexing.from_string source
  | `Path, "-" ->
      Location.input_name := "//toplevel//";
      Lexing.from_channel stdin
  | `Path, _ ->
      Location.input_name := source;
      Lexing.from_channel (wrap_open open_in source)

let () =
  Arg.parse args anon_arg usage_msg;
  if !Clflags.all_ppx = [] then begin
    Arg.usage args usage_msg;
    exit 1
  end;
  if !inputs = [] then
    inputs := [`Struct, `Path, "-"];
  let fmt =
    match !output_file with
    | "-"  -> Format.std_formatter
    | file -> Format.formatter_of_out_channel (wrap_open open_out file)
  in
  try
    !inputs |> List.iter (fun (ast_kind, source_kind, source) ->
        let lexer = make_lexer source_kind source in
        match ast_kind with
        | `Struct ->
            let pstr = Parse.implementation lexer in
            let pstr = Pparse.apply_rewriters (* ~restore:true *) ~tool_name:!tool_name
                Pparse.Structure pstr in
            Pprintast.structure fmt pstr;
            Format.pp_print_newline fmt ()
        | `Sig ->
            let psig = Parse.interface lexer in
            let psig = Pparse.apply_rewriters (* ~restore:true *) ~tool_name:!tool_name
                Pparse.Signature psig in
            Pprintast.signature fmt psig;
            Format.pp_print_newline fmt ())
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2

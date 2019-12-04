(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



type output_kind =
  | Html_output of string
  | Csv_output of string
  | Text_output of string
  | Dump_output of string
  | Coveralls_output of string

let report_outputs = ref []

let add_output o =
  report_outputs := o :: !report_outputs

let verbose = ref false

let tab_size = ref 8

let report_title = ref "Coverage report"

let csv_separator = ref ";"

let search_path = ref [""]

let add_search_path sp =
  search_path := sp :: !search_path

let raw_coverage_files = ref []

let summary_only = ref false

let ignore_missing_files = ref false

let add_file f =
  raw_coverage_files := f :: !raw_coverage_files

let service_name = ref ""

let service_job_id = ref ""

let repo_token = ref ""

let options = Arg.align [
  ("-html",
   Arg.String (fun s -> add_output (Html_output s)),
   "<dir>  Output HTML report to <dir> (HTML only)");

  ("-I",
   Arg.String add_search_path,
   "<dir>  Look for .ml files in <dir> (HTML/Coveralls only)");

  ("-ignore-missing-files",
   Arg.Set ignore_missing_files,
   " Do not fail if an .ml file can't be found (HTML/Coveralls only)");

  ("-title",
   Arg.Set_string report_title,
   "<string>  Set title for report pages (HTML only)");

  ("-tab-size",
   Arg.Int
     (fun x ->
       if x < 0 then
         (print_endline " *** error: tab size should be positive"; exit 1)
       else
         tab_size := x),
   "<int>  Set tab width in report (HTML only)");

  ("-text",
   Arg.String (fun s -> add_output (Text_output s)),
   "<file>  Output plain text report to <file>");

  ("-summary-only",
   Arg.Set summary_only,
   " Output only a whole-project summary (text only)");

  ("-csv",
   Arg.String (fun s -> add_output (Csv_output s)),
   "<file>  Output CSV report to <file>");

  ("-separator",
   Arg.Set_string csv_separator,
   "<string>  Set column separator (CSV only)");

  ("-dump",
   Arg.String (fun s -> add_output (Dump_output s)),
   "<file>  Output bare dump to <file>");

  ("-verbose",
   Arg.Set verbose,
   " Set verbose mode");

  ("-version",
   Arg.Unit (fun () -> print_endline Bisect.Version.value; exit 0),
   " Print version and exit");

  ("-coveralls",
   Arg.String (fun s -> add_output (Coveralls_output s)),
   "<file>  Output coveralls json report to <file>");

  ("-service-name",
   Arg.Set_string service_name,
   "<string>  Service name for Coveralls json (Coveralls only)");

  ("-service-job-id",
   Arg.Set_string service_job_id,
   "<string>  Service job id for Coveralls json (Coveralls only)");

  ("-repo-token",
   Arg.Set_string repo_token,
   "<string>  Repo token for Coveralls json (Coveralls only)");
]

let usage =
{|Usage:

  bisect-ppx-report <options> <.out files>

Where a file is required, '-' may be used to specify STDOUT.

Examples:

  bisect-ppx-report -html coverage/ -I _build bisect*.out
  bisect-ppx-report -text - -summary-only bisect*.out

Dune:

  bisect-ppx-report -html coverage/ -I _build/default _build/default/bisect*.out

Options are:
|}

let parse_args () = Arg.parse options add_file usage

let print_usage () = Arg.usage options usage



let main () =
  parse_args ();
  if !report_outputs = [] then begin
    print_usage ();
    exit 0
  end;
  let data, points =
    match !raw_coverage_files with
    | [] ->
        prerr_endline " *** warning: no .out files provided";
        exit 0
    | (_ :: _) ->
      let total_counts = Hashtbl.create 17 in
      let points = Hashtbl.create 17 in

      !raw_coverage_files |> List.iter (fun out_file ->
        Bisect.Common.read_runtime_data' out_file
        |> List.iter (fun (source_file, (file_counts, file_points)) ->
          let file_counts =
            let open Report_utils.Infix in
            try (Hashtbl.find total_counts source_file) +| file_counts
            with Not_found -> file_counts
          in
          Hashtbl.replace total_counts source_file file_counts;
          Hashtbl.replace points source_file file_points));

      total_counts, points
  in
  let verbose = if !verbose then print_endline else ignore in
  let search_file l f =
    let fail () =
      if !ignore_missing_files then None
      else
        raise (Sys_error (f ^ ": No such file or directory")) in
    let rec search = function
      | hd :: tl ->
          let f' = Filename.concat hd f in
          if Sys.file_exists f' then Some f' else search tl
      | [] -> fail () in
    if Filename.is_implicit f then
      search l
    else if Sys.file_exists f then
      Some f
    else
      fail () in
  let search_in_path = search_file !search_path in
  let generic_output file conv =
    Report_generic.output verbose file conv data points in
  let write_output = function
    | Html_output dir ->
        Report_utils.mkdirs dir;
        Report_html.output verbose dir
          !tab_size !report_title
          search_in_path data points
    | Csv_output file ->
        generic_output file (Report_csv.make !csv_separator)
    | Text_output file ->
        generic_output file (Report_text.make !summary_only)
    | Dump_output file ->
        generic_output file (Report_dump.make ())
    | Coveralls_output file ->
        Report_coveralls.output verbose file
          !service_name !service_job_id !repo_token
          search_in_path data points in
  List.iter write_output (List.rev !report_outputs)

let () =
  try
    main ();
    exit 0
  with
  | Sys_error s ->
      Printf.eprintf " *** system error: %s\n" s;
      exit 1
  | Unix.Unix_error (e, _, _) ->
      Printf.eprintf " *** system error: %s\n" (Unix.error_message e);
      exit 1
  | Bisect.Common.Invalid_file (f, reason) ->
      Printf.eprintf " *** invalid file: '%s' error: \"%s\"\n" f reason;
      exit 1
  | Bisect.Common.Unsupported_version s ->
      Printf.eprintf " *** unsupported file version: '%s'\n" s;
      exit 1
  | Bisect.Common.Modified_file s ->
      Printf.eprintf " *** source file modified since instrumentation: '%s'\n" s;
      exit 1
  | e ->
      Printf.eprintf " *** error: %s\n" (Printexc.to_string e);
      exit 1

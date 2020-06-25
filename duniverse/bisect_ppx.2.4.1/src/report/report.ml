(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



module Common = Bisect_common

module Arguments :
sig
  val report_outputs :
    ([ `Html | `Csv | `Text | `Dump | `Coveralls ] * string) list ref
  val verbose : bool ref
  val tab_size : int ref
  val report_title : string ref
  val csv_separator : string ref
  val search_path : string list ref
  val raw_coverage_files : string list ref
  val coverage_search_path : string list ref
  val summary_only : bool ref
  val ignore_missing_files : bool ref
  val service_name : string ref
  val service_number : string ref
  val service_job_id : string ref
  val service_pull_request : string ref
  val repo_token : string ref
  val git : bool ref
  val parallel : bool ref
  val send_to : string option ref
  val dry_run : bool ref
  val expect : string list ref
  val do_not_expect : string list ref
  val theme : Report_html.theme ref

  val parse_args : unit -> unit
  val print_usage : unit -> unit

  val is_report_being_written_to_stdout : unit -> bool
end =
struct
  let report_outputs = ref []

  let add_output o =
    report_outputs := o :: !report_outputs

  let verbose = ref false

  let tab_size = ref 8

  let report_title = ref "Coverage report"

  let csv_separator = ref ";"

  let search_path = ref ["_build/default"; ""]

  let add_search_path sp =
    search_path := sp :: !search_path

  let raw_coverage_files = ref []

  let coverage_search_path = ref []

  let summary_only = ref false

  let ignore_missing_files = ref false

  let add_file f =
    raw_coverage_files := f :: !raw_coverage_files

  let service_name = ref ""

  let service_number = ref ""

  let service_job_id = ref ""

  let service_pull_request = ref ""

  let repo_token = ref ""

  let git = ref false

  let parallel = ref false

  let send_to = ref None

  let dry_run = ref false

  let expect = ref []

  let do_not_expect = ref []

  let theme = ref `Auto

  let options = [
    ("--html",
    Arg.String (fun s -> add_output (`Html, s)),
    "<dir>  Output HTML report to <dir> (HTML only)");

    ("-I",
    Arg.String (fun s ->
      prerr_endline "bisect-ppx-report argument '-I' is deprecated.";
      prerr_endline "Use '--source-path' and the new command line instead.";
      prerr_endline "This requires Bisect_ppx >= 2.1.0.";
      add_search_path s),
    "<dir>  Look for .ml/.re files in <dir> (HTML/Coveralls only; deprecated)");

    ("--ignore-missing-files",
    Arg.Set ignore_missing_files,
    " Do not fail if an .ml/.re file can't be found (HTML/Coveralls only)");

    ("--title",
    Arg.Set_string report_title,
    "<string>  Set title for report pages (HTML only)");

    ("--tab-size",
    Arg.Int
      (fun x ->
        if x < 0 then
          (prerr_endline " *** error: tab size should be positive"; exit 1)
        else
          tab_size := x),
    "<int>  Set tab width in report (HTML only)");

    ("--text",
    Arg.String (fun s -> add_output (`Text, s)),
    "<file>  Output plain text report to <file>");

    ("--summary-only",
    Arg.Set summary_only,
    " Output only a whole-project summary (text only)");

    ("--csv",
    Arg.String (fun s -> add_output (`Csv, s)),
    "<file>  Output CSV report to <file>");

    ("--separator",
    Arg.Set_string csv_separator,
    "<string>  Set column separator (CSV only)");

    ("--dump",
    Arg.String (fun s -> add_output (`Dump, s)),
    "<file>  Output bare dump to <file>");

    ("--verbose",
    Arg.Set verbose,
    " Set verbose mode");

    ("--coveralls",
    Arg.String (fun s -> add_output (`Coveralls, s)),
    "<file>  Output coveralls json report to <file>");

    ("--service-name",
    Arg.Set_string service_name,
    "<string>  Service name for Coveralls json (Coveralls only)");

    ("--service-job-id",
    Arg.Set_string service_job_id,
    "<string>  Service job id for Coveralls json (Coveralls only)");

    ("--repo-token",
    Arg.Set_string repo_token,
    "<string>  Repo token for Coveralls json (Coveralls only)");

    ("--git",
    Arg.Set git,
    " Parse git HEAD info (Coveralls only)");

    ("--send-to",
    Arg.String (fun s -> send_to := Some s),
    "<string>  Coveralls or Codecov")
  ]

  let deprecated = Common.deprecated "bisect-ppx-report"

  let options =
    options
    |> deprecated "-html"
    |> deprecated "-ignore-missing-files"
    |> deprecated "-title"
    |> deprecated "-tab-size"
    |> deprecated "-text"
    |> deprecated "-summary-only"
    |> deprecated "-csv"
    |> deprecated "-separator"
    |> deprecated "-dump"
    |> deprecated "-verbose"
    |> deprecated "-coveralls"
    |> deprecated "-service-name"
    |> deprecated "-service-job-id"
    |> deprecated "-repo-token"
    |> Arg.align

  let usage =
{|This is the legacy command line. Please see

  bisect-ppx-report --help

Options are:
|}

  let parse_args () = Arg.parse options add_file usage

  let print_usage () = Arg.usage options usage

  let is_report_being_written_to_stdout () =
    !report_outputs |> List.exists (fun (_, file) -> file = "-")
end



let quiet =
  ref false

let info arguments =
  Printf.ksprintf (fun s ->
    if not !quiet then
      Printf.printf "Info: %s\n%!" s) arguments

let warning =
  Printf.ksprintf (fun s ->
    Printf.eprintf "Warning: %s\n%!" s)

let error arguments =
  Printf.ksprintf (fun s ->
    Printf.eprintf "Error: %s\n%!" s; exit 1) arguments



module Coverage_input_files :
sig
  val list : unit -> string list
  val expected_sources_are_present : string list -> unit
end =
struct
  let has_extension extension filename =
    Filename.check_suffix filename extension

  let list_recursively directory filename_filter =
    let rec traverse directory files =
      Sys.readdir directory
      |> Array.fold_left begin fun files entry ->
        let entry_path = Filename.concat directory entry in
        if Sys.is_directory entry_path then
          traverse entry_path files
        else
          if filename_filter entry_path entry then
            entry_path::files
          else
            files
      end files
    in
    traverse directory []

  let filename_filter path filename =
    if has_extension ".coverage" filename then
      true
    else
      if has_extension ".out" filename then
        let prefix = "bisect" in
        match String.sub filename 0 (String.length prefix) with
        | prefix' when prefix' = prefix ->
          warning
            "found file '%s': Bisect_ppx 2.x uses extension '.coverage'"
            path;
          true
        | _ ->
          false
        | exception Invalid_argument _ ->
          false
      else
        false

  let list () =
    let files_on_command_line = !Arguments.raw_coverage_files in

    (* Check for .out files on the command line. If there is such a file, it is
       most likely an unexpaned pattern bisect*.out, from a former user of
       Bisect_ppx 1.x. *)
    begin match List.find (has_extension ".out") files_on_command_line with
    | exception Not_found -> ()
    | filename ->
      warning
        "file '%s' on command line: Bisect_ppx 2.x uses extension '.coverage'"
        filename
    end;

    (* If there are files on the command line, or coverage search directories
       specified, use those. Otherwise, search for files in ./ and ./_build.
       During the search, we look for files with extension .coverage. If we find
       any files bisect*.out, we display a warning. *)
    let all_coverage_files =
      match files_on_command_line, !Arguments.coverage_search_path with
      | [], [] ->
        let in_current_directory =
          Sys.readdir Filename.current_dir_name
          |> Array.to_list
          |> List.filter (fun entry ->
            filename_filter (Filename.(concat current_dir_name) entry) entry)
        in
        let in_build_directory =
          if Sys.file_exists "_build" && Sys.is_directory "_build" then
            list_recursively "./_build" filename_filter
          else
            []
        in
        let in_esy_sandbox =
          match Sys.getenv "cur__target_dir" with
          | exception Not_found -> []
          | directory -> list_recursively directory filename_filter
        in
        in_current_directory @ in_build_directory @ in_esy_sandbox

      | _ ->
        !Arguments.coverage_search_path
        |> List.filter Sys.file_exists
        |> List.filter Sys.is_directory
        |> List.map (fun dir -> list_recursively dir filename_filter)
        |> List.flatten
        |> (@) files_on_command_line
    in

    begin
      match files_on_command_line, !Arguments.coverage_search_path with
    | [], [] | _, _::_ ->
      (* Display feedback about where coverage files were found. *)
      all_coverage_files
      |> List.map Filename.dirname
      |> List.sort_uniq String.compare
      |> List.map (fun directory -> directory ^ Filename.dir_sep)
      |> List.iter (info "found coverage files in '%s'")
    | _ ->
      ()
    end;

    if all_coverage_files = [] then
      error "no coverage files given on command line or found"
    else
      all_coverage_files

  let strip_extensions filename =
    let dirname, basename = Filename.(dirname filename, basename filename) in
    let basename =
      match String.index basename '.' with
      | index -> String.sub basename 0 index
      | exception Not_found -> basename
    in
    Filename.concat dirname basename

  let list_expected_files paths =
    paths
    |> List.map (fun path ->
      if Filename.(check_suffix path dir_sep) then
        list_recursively path (fun _path filename ->
          [".ml"; ".re"; ".mll"; ".mly"]
          |> List.exists (Filename.check_suffix filename))
      else
        [path])
    |> List.flatten
    |> List.sort_uniq String.compare

  let filtered_expected_files () =
    let expected_files = list_expected_files !Arguments.expect in
    let excluded_files = list_expected_files !Arguments.do_not_expect in
    expected_files
    |> List.filter (fun path -> not (List.mem path excluded_files))
    (* Not the fastest way. *)

  let expected_sources_are_present present_files =
    let present_files = List.map strip_extensions present_files in
    let expected_files = filtered_expected_files () in
    expected_files |> List.iter (fun file ->
      if not (List.mem (strip_extensions file) present_files) then
        error "expected file '%s' is not included in the report" file)
end



type ci = [
  | `CircleCI
  | `Travis
  | `GitHub
]

module CI :
sig
  val detect : unit -> ci option
  val pretty_name : ci -> string
  val name_in_report : ci -> string
  val job_id_variable : ci -> string
end =
struct
  let environment_variable name value result k =
    match Sys.getenv name with
    | value' when value' = value -> Some result
    | _ -> k ()
    | exception Not_found -> k ()

  let detect () =
    environment_variable "CIRCLECI" "true" `CircleCI @@ fun () ->
    environment_variable "TRAVIS" "true" `Travis @@ fun () ->
    environment_variable "GITHUB_ACTIONS" "true" `GitHub @@ fun () ->
    None

  let pretty_name = function
    | `CircleCI -> "CircleCI"
    | `Travis -> "Travis"
    | `GitHub -> "GitHub Actions"

  let name_in_report = function
    | `CircleCI -> "circleci"
    | `Travis -> "travis-ci"
    | `GitHub -> "github"

  let job_id_variable = function
    | `CircleCI -> "CIRCLE_BUILD_NUM"
    | `Travis -> "TRAVIS_JOB_ID"
    | `GitHub -> "GITHUB_RUN_NUMBER"
end



type coverage_service = [
  | `Codecov
  | `Coveralls
]

module Coverage_service :
sig
  val from_argument : unit -> coverage_service option
  val pretty_name : coverage_service -> string
  val report_filename : coverage_service -> string
  val send_command : coverage_service -> string
  val needs_pull_request_number : ci -> coverage_service -> string option
  val needs_repo_token : ci -> coverage_service -> bool
  val repo_token_variables : coverage_service -> string list
  val needs_git_info : ci -> coverage_service -> bool
end =
struct
  let from_argument () =
    match !Arguments.send_to with
    | None -> None
    | Some "Codecov" -> Some `Codecov
    | Some "Coveralls" -> Some `Coveralls
    | Some other -> error "send-to: unknown coverage service '%s'" other

  let pretty_name = function
    | `Codecov -> "Codecov"
    | `Coveralls -> "Coveralls"

  let report_filename _ =
    "coverage.json"

  let send_command = function
    | `Codecov ->
      "curl -s https://codecov.io/bash | bash -s -- -Z -f coverage.json"
    | `Coveralls ->
      "curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs"

  let needs_pull_request_number ci service =
    match ci, service with
    | `CircleCI, `Coveralls -> Some "CIRCLE_PULL_REQUEST"
    | `GitHub, `Coveralls -> Some "PULL_REQUEST_NUMBER"
    | _ -> None

  let needs_repo_token ci service =
    match ci, service with
    | `CircleCI, `Coveralls -> true
    | `GitHub, `Coveralls -> true
    | _ -> false

  let repo_token_variables = function
    | `Codecov -> ["CODECOV_TOKEN"]
    | `Coveralls -> ["COVERALLS_REPO_TOKEN"]

  let needs_git_info ci service =
    match ci, service with
    | `CircleCI, `Coveralls -> true
    | `GitHub, `Coveralls -> true
    | _ -> false
end



let main () =
  quiet := Arguments.is_report_being_written_to_stdout ();

  let coverage_service = Coverage_service.from_argument () in

  begin match coverage_service with
  | None ->
    ()
  | Some service ->
    let report_file = Coverage_service.report_filename service in
    info "will write coverage report to '%s'" report_file;
    Arguments.report_outputs :=
      !Arguments.report_outputs @ [`Coveralls, report_file];

    let ci =
      lazy begin
        match CI.detect () with
        | Some ci ->
          info "detected CI: %s" (CI.pretty_name ci);
          ci
        | None ->
          error "unknown CI service or not in CI"
      end
    in

    if !Arguments.service_name = "" then begin
      let service_name = CI.name_in_report (Lazy.force ci) in
      info "using service name '%s'" service_name;
      Arguments.service_name := service_name;
    end;

    if !Arguments.service_job_id = "" then begin
      let job_id_variable = CI.job_id_variable (Lazy.force ci) in
      info "using job ID variable $%s" job_id_variable;
      match Sys.getenv job_id_variable with
      | value ->
        Arguments.service_job_id := value
      | exception Not_found ->
        error "expected job id in $%s" job_id_variable
    end;

    if !Arguments.service_pull_request = "" then begin
      let needs =
        Coverage_service.needs_pull_request_number (Lazy.force ci) service in
      match needs with
      | None ->
        ()
      | Some pr_variable ->
        match Sys.getenv pr_variable with
        | value ->
          info "using PR number variable $%s" pr_variable;
          Arguments.service_pull_request := value
        | exception Not_found ->
          info "$%s not set" pr_variable
    end;

    if !Arguments.repo_token = "" then
      if Coverage_service.needs_repo_token (Lazy.force ci) service then begin
        let repo_token_variables =
          Coverage_service.repo_token_variables service in
        let rec try_variables = function
          | variable::more ->
            begin match Sys.getenv variable with
            | exception Not_found ->
              try_variables more
            | value ->
              info "using repo token variable $%s" variable;
              Arguments.repo_token := value
            end
          | [] ->
            error "expected repo token in $%s" (List.hd repo_token_variables)
        in
        try_variables repo_token_variables
      end;

    if not !Arguments.git then
      if Coverage_service.needs_git_info (Lazy.force ci) service then begin
        info "including git info";
        Arguments.git := true
      end
  end;

  let data, points =
    let total_counts = Hashtbl.create 17 in
    let points = Hashtbl.create 17 in

    Coverage_input_files.list () |> List.iter (fun out_file ->
      Common.read_runtime_data out_file
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

  let present_files =
    Hashtbl.fold (fun file _ acc -> file::acc) data [] in
  Coverage_input_files.expected_sources_are_present present_files;

  let verbose = if !Arguments.verbose then print_endline else ignore in
  let search_file l f =
    let fail () =
      if !Arguments.ignore_missing_files then None
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
  let search_in_path = search_file !Arguments.search_path in
  let generic_output file conv =
    Report_generic.output verbose file conv data points in
  let write_output = function
    | `Html, dir ->
      Report_utils.mkdirs dir;
      Report_html.output verbose dir
        !Arguments.tab_size !Arguments.report_title !Arguments.theme
          search_in_path data points
    | `Csv, file ->
      generic_output file (Report_csv.make !Arguments.csv_separator)
    | `Text, "-" ->
      Report_text.output ~per_file:(not !Arguments.summary_only) data
    | `Text, file ->
      generic_output file (Report_text.make !Arguments.summary_only)
    | `Dump, file ->
      generic_output file (Report_dump.make ())
    | `Coveralls, file ->
      Report_coveralls.output verbose file
        !Arguments.service_name
        !Arguments.service_number
        !Arguments.service_job_id
        !Arguments.service_pull_request
        !Arguments.repo_token
        !Arguments.git
        !Arguments.parallel
        search_in_path data points
  in
  List.iter write_output (List.rev !Arguments.report_outputs);

  match coverage_service with
  | None ->
    ()
  | Some coverage_service ->
    let name = Coverage_service.pretty_name coverage_service in
    let command = Coverage_service.send_command coverage_service in
    info "sending to %s with command:" name;
    info "%s" command;
    if not !Arguments.dry_run then begin
      let exit_code = Sys.command command in
      let report = Coverage_service.report_filename coverage_service in
      if Sys.file_exists report then begin
        info "deleting '%s'" report;
        Sys.remove report
      end;
      exit exit_code
    end



let main () =
  try
    main ()
  with
  | Sys_error s ->
    Printf.eprintf " *** system error: %s\n" s;
    exit 1
  | Unix.Unix_error (e, _, _) ->
    Printf.eprintf " *** system error: %s\n" (Unix.error_message e);
    exit 1
  | Common.Invalid_file (f, reason) ->
    Printf.eprintf " *** invalid file: '%s' error: \"%s\"\n" f reason;
    exit 1
  | e ->
    Printf.eprintf " *** error: %s\n" (Printexc.to_string e);
    exit 1



module Command_line :
sig
  val eval : unit -> unit
end =
struct
  let esy_source_dir =
    match Sys.getenv "cur__target_dir" with
    | exception Not_found -> []
    | directory -> [Filename.concat directory "default"]

  open Cmdliner

  let (-->) a f = Term.(const f $ a)
  let (&&&) a b = Term.(const (fun () () -> ()) $ a $ b)
  let main' = Term.(app (const main))
  let term_info = Term.info ~sdocs:"COMMON OPTIONS"

  let coverage_files from_position =
    Arg.(value @@ pos_right (from_position - 1) string [] @@
      info [] ~docv:"COVERAGE_FILES" ~doc:
        ("Optional list of *.coverage files produced during testing. If not " ^
        "specified, and $(b,--coverage-path) is also not specified, " ^
        "bisect-ppx-report will search for *.coverage files non-recursively " ^
        "in ./ and recursively in ./_build, and, if run under esy, inside " ^
        "the esy sandbox."))
    --> (:=) Arguments.raw_coverage_files

  let coverage_search_directories =
    Arg.(value @@ opt_all string [] @@
      info ["coverage-path"] ~docv:"DIRECTORY" ~doc:
        ("Directory in which to look for .coverage files. This option can be " ^
        "specified multiple times. The search is recursive in each directory."))
    --> (:=) Arguments.coverage_search_path

  let output_file kind =
    Arg.(required @@ pos 0 (some string) None @@
      info [] ~docv:"FILE" ~doc:"Output file name.")
    --> fun f -> Arguments.report_outputs := [kind, f]

  let search_directories =
    Arg.(value @@ opt_all string (["."; "./_build/default"] @ esy_source_dir) @@
      info ["source-path"] ~docv:"DIRECTORY" ~doc:
        ("Directory in which to look for source files. This option can be " ^
        "specified multiple times. File paths are concatenated with each " ^
        "$(b,--source-path) directory when looking for files. The default " ^
        "directories are ./ and ./_build/default/. If running inside an esy " ^
        "sandbox, the default/ directory in the sandbox is also included."))
    --> (:=) Arguments.search_path

  let ignore_missing_files =
    Arg.(value @@ flag @@
      info ["ignore-missing-files"] ~doc:
        "Do not fail if a particular .ml or .re file can't be found.")
    --> (:=) Arguments.ignore_missing_files

  let service_name =
    Arg.(value @@ opt string "" @@
      info ["service-name"] ~docv:"STRING" ~doc:
        "Include \"service_name\": \"$(i,STRING)\" in the generated report.")
    --> (:=) Arguments.service_name

  let service_number =
    Arg.(value @@ opt string "" @@
      info ["service-number"] ~docv:"STRING" ~doc:
        "Include \"service_number\": \"$(i,STRING)\" in the generated report.")
    --> (:=) Arguments.service_number

  let service_job_id =
    Arg.(value @@ opt string "" @@
      info ["service-job-id"] ~docv:"STRING" ~doc:
        "Include \"service_job_id\": \"$(i,STRING)\" in the generated report.")
    --> (:=) Arguments.service_job_id

  let service_pull_request =
    Arg.(value @@ opt string "" @@
      info ["service-pull-request"] ~docv:"STRING" ~doc:
        ("Include \"service_pull_request\": \"$(i,STRING)\" in the generated " ^
        "report."))
    --> (:=) Arguments.service_pull_request

  let repo_token =
    Arg.(value @@ opt string "" @@
      info ["repo-token"] ~docv:"STRING" ~doc:
        "Include \"repo_token\": \"$(i,STRING)\" in the generated report.")
    --> (:=) Arguments.repo_token

  let git =
    Arg.(value @@ flag @@
      info ["git"] ~doc:"Include git commit info in the generated report.")
    --> (:=) Arguments.git

  let parallel =
    Arg.(value @@ flag @@
      info ["parallel"] ~doc:
        "Include \"parallel\": true in the generated report.")
    --> (:=) Arguments.parallel

  let expect =
    Arg.(value @@ opt_all string [] @@
      info ["expect"] ~docv:"PATH" ~docs:"COMMON OPTIONS" ~doc:
        ("Check that the files at $(i,PATH) are included in the coverage " ^
        "report. This option can be given multiple times. If $(i,PATH) ends " ^
        "with a path separator (slash), it is treated as a directory name. " ^
        "The reporter scans the directory recursively, and expects all files " ^
        "in the directory to appear in the report. If $(i,PATH) does not end " ^
        "with a path separator, it is treated as the name of a single file, " ^
        "and the reporter expects that file to appear in the report. In both " ^
        "cases, files expected are limited to those with extensions .ml, " ^
        ".re, .mll, and .mly. When matching files, extensions are stripped, " ^
        "including nested .cppo extensions."))
    --> (:=) Arguments.expect

  let do_not_expect =
    Arg.(value @@ opt_all string [] @@
      info ["do-not-expect"] ~docv:"PATH" ~docs:"COMMON OPTIONS" ~doc:
        ("Excludes files from those specified with $(b,--expect). This " ^
        "option can be given multiple times. If $(i,PATH) ends with a path " ^
        "separator (slash), it is treated as a directory name. All files " ^
        "found recursively in the directory are then not required to appear " ^
        "in the report. If $(i,PATH) does not end with a path separator, it " ^
        "is treated as the name of a single file, and that file is not " ^
        "required to appear in the report."))
    --> (:=) Arguments.do_not_expect

  let html =
    let output_directory =
      Arg.(value @@ opt string "./_coverage" @@
        info ["o"] ~docv:"DIRECTORY" ~doc:"Output directory.")
      --> fun d -> Arguments.report_outputs := [`Html, d]
    in
    let title =
      Arg.(value @@ opt string "Coverage report" @@
        info ["title"] ~docv:"STRING" ~doc:
          "Report title for use in HTML pages.")
      --> (:=) Arguments.report_title
    in
    let tab_size =
      Arg.(value @@ opt int 2 @@
        info ["tab-size"] ~docv:"N" ~doc:
          "Set TAB width for replacing TAB characters in HTML pages.")
      --> (:=) Arguments.tab_size
    in
    let theme =
      Arg.(value @@
        opt (enum ["light", `Light; "dark", `Dark; "auto", `Auto]) `Auto @@
        info ["theme"] ~docv:"THEME" ~doc:
          ("$(i,light) or $(i,dark). The default value, $(i,auto), causes " ^
          "the report's theme to adapt to system or browser preferences."))
      --> (:=) Arguments.theme
    in
    output_directory &&&
    coverage_files 0 &&&
    coverage_search_directories &&&
    search_directories &&&
    ignore_missing_files &&&
    title &&&
    tab_size &&&
    theme &&&
    expect &&&
    do_not_expect
    |> main',
    term_info "html" ~doc:"Generate HTML report locally."
      ~man:[
        `S "USAGE EXAMPLE";
        `P "Run";
        `Pre "    bisect-ppx-report html";
        `P
          ("Then view the generated report at _coverage/index.html with your " ^
          "browser. All arguments are optional.")
      ]

  let send_to =
    let service =
      Arg.(required @@ pos 0 (some string) None @@
        info [] ~docv:"SERVICE" ~doc:"'Coveralls' or 'Codecov'.")
      --> fun s -> Arguments.send_to := Some s
    in
    let dry_run =
      Arg.(value @@ flag @@
        info ["dry-run"] ~doc:
          ("Don't issue the final upload command and don't delete the " ^
          "intermediate coverage report file."))
      --> (:=) Arguments.dry_run
    in
    service &&&
    coverage_files 1 &&&
    coverage_search_directories &&&
    search_directories &&&
    ignore_missing_files &&&
    service_name &&&
    service_number &&&
    service_job_id &&&
    service_pull_request &&&
    repo_token &&&
    git &&&
    parallel &&&
    dry_run &&&
    expect &&&
    do_not_expect
    |> main',
    term_info "send-to" ~doc:"Send report to a supported web service."
      ~man:[`S "USAGE EXAMPLE"; `Pre "bisect-ppx-report send-to Coveralls"]

  let text =
    let per_file =
      Arg.(value @@ flag @@
        info ["per-file"] ~doc:"Include coverage per source file.")
      --> fun b -> Arguments.summary_only := not b
    in
    Term.const () --> (fun () -> Arguments.report_outputs := [`Text, "-"]) &&&
    coverage_files 0 &&&
    coverage_search_directories &&&
    per_file &&&
    expect &&&
    do_not_expect
    |> main',
    term_info "summary" ~doc:"Write coverage summary to STDOUT."

  let coveralls =
    output_file `Coveralls &&&
    coverage_files 1 &&&
    coverage_search_directories &&&
    search_directories &&&
    ignore_missing_files &&&
    service_name &&&
    service_number &&&
    service_job_id &&&
    service_pull_request &&&
    repo_token &&&
    git &&&
    parallel &&&
    expect &&&
    do_not_expect
    |> main',
    term_info "coveralls" ~doc:
      ("Generate Coveralls JSON report (for manual integration with web " ^
      "services).")

  let csv =
    let separator =
      Arg.(value @@ opt string ";" @@
        info ["separator"] ~docv:"STRING" ~doc:"Field separator to use.")
      --> (:=) Arguments.csv_separator
    in
    output_file `Csv &&&
    coverage_files 1 &&&
    coverage_search_directories &&&
    separator &&&
    expect &&&
    do_not_expect
    |> main',
    term_info "csv" ~doc:"(Debug) Generate CSV report."

  let dump =
    output_file `Dump &&&
    coverage_files 1 &&&
    coverage_search_directories &&&
    expect &&&
    do_not_expect
    |> main',
    term_info "dump" ~doc:"(Debug) Dump binary report."

  let ordinary_subcommands =
    [html; send_to; text; coveralls]

  let debug_subcommands =
    [csv; dump]

  let all_subcommands =
    ordinary_subcommands @ debug_subcommands

  let reporter () =
    Term.(eval_choice
      (ret (const (`Help (`Auto, None))),
      term_info
        "bisect-ppx-report"
        ~doc:"Generate coverage reports for OCaml and Reason."
        ~man:[
          `S "USAGE EXAMPLE";
          `Pre
            ("bisect-ppx-report html\nbisect-ppx-report send-to Coveralls\n" ^
            "bisect-ppx-report summary");
          `P
            ("See bisect-ppx-report $(i,COMMAND) --help for further " ^
            "information on each command, including options.")
        ]))
      ordinary_subcommands

  let eval () =
    let is_legacy_command_line =
      let subcommand_names =
        List.map (fun (_, info) -> Term.name info) all_subcommands in
      match List.mem Sys.argv.(1) ("--help"::subcommand_names) with
      | result -> not result
      | exception Invalid_argument _ -> false
    in

    if is_legacy_command_line then begin
      warning
        "you are using the old command-line syntax. %s"
        "See bisect-ppx-report --help";
      Arguments.parse_args ();
      if !Arguments.report_outputs = [] && !Arguments.send_to = None then begin
        Arguments.print_usage ();
        exit 0
      end;
      main ()
    end
    else
      Term.exit (reporter ())
end



let () =
  Command_line.eval ()

(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



module Common = Bisect_common

module Arguments :
sig
  val report_outputs : ([ `Html | `Text | `Coveralls ] * string) list ref
  val verbose : bool ref
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

  val is_report_being_written_to_stdout : unit -> bool
end =
struct
  let report_outputs = ref []

  let verbose = ref false

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

  let is_report_being_written_to_stdout () =
    !report_outputs |> List.exists (fun (_, file) -> file = "-")
end



let quiet =
  ref false

let info arguments =
  Printf.ksprintf (fun s ->
    if not !quiet then
      Printf.printf "Info: %s\n%!" s) arguments

let error arguments =
  Printf.ksprintf (fun s ->
    Printf.eprintf "Error: %s\n%!" s; exit 1) arguments



module Coverage_input_files :
sig
  val list : string list -> string list -> string list
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
        match Sys.is_directory entry_path with
        | true ->
          traverse entry_path files
        | false ->
          if filename_filter entry_path entry then
            entry_path::files
          else
            files
        | exception Sys_error _ ->
          files
      end files
    in
    traverse directory []

  let filename_filter _path filename =
    has_extension ".coverage" filename

  let list files_on_command_line coverage_search_paths =
    (* If there are files on the command line, or coverage search directories
       specified, use those. Otherwise, search for files in ./ and ./_build.
       During the search, we look for files with extension .coverage. *)
    let all_coverage_files =
      match files_on_command_line, coverage_search_paths with
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
          | directory ->
            if Sys.file_exists directory && Sys.is_directory directory then
              list_recursively directory filename_filter
            else
              []
        in
        in_current_directory @ in_build_directory @ in_esy_sandbox

      | _ ->
        coverage_search_paths
        |> List.filter Sys.file_exists
        |> List.filter Sys.is_directory
        |> List.map (fun dir -> list_recursively dir filename_filter)
        |> List.flatten
        |> (@) files_on_command_line
    in

    begin
      match files_on_command_line, coverage_search_paths with
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



let load_coverage files search_paths =
  let data, points =
    let total_counts = Hashtbl.create 17 in
    let points = Hashtbl.create 17 in

    Coverage_input_files.list files search_paths
    |> List.iter (fun out_file ->
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

  data, points



let search_file l ignore_missing_files f =
  let fail () =
    if ignore_missing_files then None
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
    fail ()



let html
    dir title tab_size theme coverage_files coverage_paths source_paths
    ignore_missing_files () =

  let data, points = load_coverage coverage_files coverage_paths in
  let verbose = if !Arguments.verbose then print_endline else ignore in
  let search_in_path = search_file source_paths ignore_missing_files in
  Report_utils.mkdirs dir;
  Report_html.output verbose dir tab_size title theme search_in_path data points



let text per_file coverage_files coverage_paths () =
  quiet := true;
  let data, _ = load_coverage coverage_files coverage_paths in
  Report_text.output ~per_file data



let coveralls
    file coverage_files coverage_paths search_path ignore_missing_files () =

  let coverage_service = Coverage_service.from_argument () in

  let file =
    match coverage_service with
    | None ->
      file
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
        end;

      report_file
  in

  let data, points = load_coverage coverage_files coverage_paths in

  let verbose = if !Arguments.verbose then print_endline else ignore in
  let search_in_path = search_file search_path ignore_missing_files in

  Report_coveralls.output verbose file
    !Arguments.service_name
    !Arguments.service_number
    !Arguments.service_job_id
    !Arguments.service_pull_request
    !Arguments.repo_token
    !Arguments.git
    !Arguments.parallel
    search_in_path data points;

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
  let term_info = Term.info ~sdocs:"COMMON OPTIONS"

  let coverage_files from_position =
    Arg.(value @@ pos_right (from_position - 1) string [] @@
      info [] ~docv:"COVERAGE_FILES" ~doc:
        ("Optional list of *.coverage files produced during testing. If not " ^
        "specified, and $(b,--coverage-path) is also not specified, " ^
        "bisect-ppx-report will search for *.coverage files non-recursively " ^
        "in ./ and recursively in ./_build, and, if run under esy, inside " ^
        "the esy sandbox."))

  let coverage_paths =
    Arg.(value @@ opt_all string [] @@
      info ["coverage-path"] ~docv:"DIRECTORY" ~doc:
        ("Directory in which to look for .coverage files. This option can be " ^
        "specified multiple times. The search is recursive in each directory."))

  let output_file =
    Arg.(required @@ pos 0 (some string) None @@
      info [] ~docv:"FILE" ~doc:"Output file name.")

  let source_paths =
    Arg.(value @@ opt_all string (["."; "./_build/default"] @ esy_source_dir) @@
      info ["source-path"] ~docv:"DIRECTORY" ~doc:
        ("Directory in which to look for source files. This option can be " ^
        "specified multiple times. File paths are concatenated with each " ^
        "$(b,--source-path) directory when looking for files. The default " ^
        "directories are ./ and ./_build/default/. If running inside an esy " ^
        "sandbox, the default/ directory in the sandbox is also included."))

  let ignore_missing_files =
    Arg.(value @@ flag @@
      info ["ignore-missing-files"] ~doc:
        "Do not fail if a particular .ml or .re file can't be found.")

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
    in
    let title =
      Arg.(value @@ opt string "Coverage report" @@
        info ["title"] ~docv:"STRING" ~doc:
          "Report title for use in HTML pages.")
    in
    let tab_size =
      Arg.(value @@ opt int 2 @@
        info ["tab-size"] ~docv:"N" ~doc:
          "Set TAB width for replacing TAB characters in HTML pages.")
    in
    let theme =
      Arg.(value @@
        opt (enum ["light", `Light; "dark", `Dark; "auto", `Auto]) `Auto @@
        info ["theme"] ~docv:"THEME" ~doc:
          ("$(i,light) or $(i,dark). The default value, $(i,auto), causes " ^
          "the report's theme to adapt to system or browser preferences."))
    in
    expect &&&
    do_not_expect
    |> Term.(app (const html
      $ output_directory $ title $ tab_size $ theme $ coverage_files 0
      $ coverage_paths $ source_paths $ ignore_missing_files)),
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
    |> Term.(app (const coveralls
      $ const "" $ coverage_files 1 $ coverage_paths $ source_paths
      $ ignore_missing_files)),
    term_info "send-to" ~doc:"Send report to a supported web service."
      ~man:[`S "USAGE EXAMPLE"; `Pre "bisect-ppx-report send-to Coveralls"]

  let text =
    let per_file =
      Arg.(value @@ flag @@
        info ["per-file"] ~doc:"Include coverage per source file.")
    in
    expect &&&
    do_not_expect
    |> Term.(app (const text
      $ per_file $ coverage_files 0 $ coverage_paths)),
    term_info "summary" ~doc:"Write coverage summary to STDOUT."

  let coveralls =
    service_name &&&
    service_number &&&
    service_job_id &&&
    service_pull_request &&&
    repo_token &&&
    git &&&
    parallel &&&
    expect &&&
    do_not_expect
    |> Term.(app (const coveralls
      $ output_file $ coverage_files 1 $ coverage_paths $ source_paths
      $ ignore_missing_files)),
    term_info "coveralls" ~doc:
      ("Generate Coveralls JSON report (for manual integration with web " ^
      "services).")

  let eval () =
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
      [html; send_to; text; coveralls]
    |> Term.exit
end



let () =
  Command_line.eval ()

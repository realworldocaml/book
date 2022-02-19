(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(* The actual Coveralls report. *)

let file_json indent resolver {Bisect_common.filename; points; counts} =
  Util.info "Processing file '%s'..." filename;
  match resolver ~filename with
  | None ->
    None
  | Some resolved_in_file ->
    let digest = Digest.to_hex (Digest.file resolved_in_file) in
    let line_counts =
      Util.line_counts ~filename:resolved_in_file ~points ~counts in
    let scounts =
      line_counts
      |> List.map (function
        | None -> "null"
        | Some nb -> Printf.sprintf "%d" nb)
    in
    let coverage = String.concat "," scounts in
    let indent_strings indent l =
      let i = String.make indent ' ' in
      List.map (fun s -> i ^ s) l
    in
    Some begin
      [
        "{";
        Printf.sprintf "    \"name\": \"%s\"," filename;
        Printf.sprintf "    \"source_digest\": \"%s\"," digest;
        Printf.sprintf "    \"coverage\": [%s]" coverage;
        "}";
      ]
      |> indent_strings indent
      |> String.concat "\n"
    end

let output_of command =
  let channel = Unix.open_process_in command in
  let line = input_line channel in
  match Unix.close_process_in channel with
  | WEXITED 0 ->
    line
  | _ ->
    Printf.eprintf "Error: command failed: '%s'\n%!" command;
    exit 1

let metadata name field =
  output_of ("git log -1 --pretty=format:'" ^ field ^ "'")
  |> String.escaped
  |> Printf.sprintf "\"%s\":\"%s\"" name

let output
    ~to_file ~service_name ~service_number ~service_job_id ~service_pull_request
    ~repo_token ~git ~parallel ~coverage_files ~coverage_paths ~source_paths
    ~ignore_missing_files ~expect ~do_not_expect =

  let coverage =
    Input.load_coverage
      ~coverage_files ~coverage_paths ~expect ~do_not_expect in
  let resolver =
    Util.find_source_file ~source_roots:source_paths ~ignore_missing_files in

  let git =
    if not git then
      ""
    else
      let metadata =
        String.concat "," [
          metadata "id" "%H";
          metadata "author_name" "%an";
          metadata "author_email" "%ae";
          metadata "committer_name" "%cn";
          metadata "committer_email" "%ce";
          metadata "message" "%s";
        ]
      in
      let branch = output_of "git rev-parse --abbrev-ref HEAD" in
      Printf.sprintf
        "    \"git\":{\"head\":{%s},\"branch\":\"%s\",\"remotes\":{}},"
        metadata branch
  in

  let file_jsons =
    Hashtbl.fold begin fun _ file acc ->
      let maybe_json = file_json 8 resolver file in
      match maybe_json with
      | None -> acc
      | Some s -> s::acc
    end coverage []
  in
  let repo_params =
    [
      "service_name", (String.trim service_name);
      "service_number", (String.trim service_number);
      "service_job_id", (String.trim service_job_id);
      "service_pull_request", (String.trim service_pull_request);
      "repo_token", (String.trim repo_token);
    ]
    |> List.filter (fun (_, v) -> (String.length v) > 0)
    |> List.map (fun (n, v) -> Printf.sprintf "    \"%s\": \"%s\"," n v)
    |> String.concat "\n"
  in
  let parallel =
    if parallel then
      "    \"parallel\": true,"
    else
      ""
  in

  Util.mkdirs (Filename.dirname to_file);
  let ch =
    try open_out to_file
    with Sys_error message ->
      Util.fatal "cannot open output file '%s': %s" to_file message
  in
  try
    Printf.fprintf ch {|{
%s
%s
%s
  "source_files": [
%s
  ]
}
|}
      repo_params
      git
      parallel
      (String.concat ",\n" file_jsons);
    close_out ch

  with
  | Sys_error message ->
    Util.fatal "cannot write output file '%s': %s" to_file message
  | exn ->
    close_out_noerr ch;
    raise exn



(* Automatically detecting the CI and sending the report to third-party
   services. *)

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
  val pretty_name : coverage_service -> string
  val report_filename : coverage_service -> string
  val send_command : coverage_service -> string
  val needs_pull_request_number : ci -> coverage_service -> string option
  val needs_repo_token : ci -> coverage_service -> bool
  val repo_token_variables : coverage_service -> string list
  val needs_git_info : ci -> coverage_service -> bool
end =
struct
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

let output_and_send
    ~service ~service_name ~service_number ~service_job_id ~service_pull_request
    ~repo_token ~git ~parallel ~dry_run ~coverage_files ~coverage_paths
    ~source_paths ~ignore_missing_files ~expect ~do_not_expect =

  let to_file = Coverage_service.report_filename service in
  Util.info "will write coverage report to '%s'" to_file;

  let ci =
    lazy begin
      match CI.detect () with
      | Some ci ->
        Util.info "detected CI: %s" (CI.pretty_name ci);
        ci
      | None ->
        Util.fatal "unknown CI service or not in CI"
    end
  in

  let or_default default string =
    if string <> "" then
      string
    else
      default ()
  in

  let service_name =
    service_name |> or_default (fun () -> CI.name_in_report (Lazy.force ci)) in
  Util.info "using service name '%s'" service_name;

  let service_job_id =
    service_job_id |> or_default begin fun () ->
      let job_id_variable = CI.job_id_variable (Lazy.force ci) in
      Util.info "using job ID variable $%s" job_id_variable;
      match Sys.getenv job_id_variable with
      | value ->
        value
      | exception Not_found ->
        Util.fatal "expected job id in $%s" job_id_variable
    end
  in

  let service_pull_request =
    service_pull_request |> or_default begin fun () ->
      let needs =
        Coverage_service.needs_pull_request_number (Lazy.force ci) service in
      match needs with
      | None ->
        ""
      | Some pr_variable ->
        match Sys.getenv pr_variable with
        | value ->
          Util.info "using PR number variable $%s" pr_variable;
          value
        | exception Not_found ->
          Util.info "$%s not set" pr_variable;
          ""
    end
  in

  let repo_token =
    repo_token |> or_default begin fun () ->
      if Coverage_service.needs_repo_token (Lazy.force ci) service then begin
        let repo_token_variables =
          Coverage_service.repo_token_variables service in
        let rec try_variables = function
          | variable::more ->
            begin match Sys.getenv variable with
            | exception Not_found ->
              try_variables more
            | value ->
              Util.info "using repo token variable $%s" variable;
              value
            end
          | [] ->
            Util.fatal
              "expected repo token in $%s" (List.hd repo_token_variables)
        in
        try_variables repo_token_variables
      end
      else
        ""
    end
  in

  let git =
    if not git then
      if Coverage_service.needs_git_info (Lazy.force ci) service then begin
        Util.info "including git info";
        true
      end
      else
        false
    else
      false
  in

  output
    ~to_file ~service_name ~service_number ~service_job_id ~service_pull_request
    ~repo_token ~git ~parallel ~coverage_files ~coverage_paths ~source_paths
    ~ignore_missing_files ~expect ~do_not_expect;

  let name = Coverage_service.pretty_name service in
  let command = Coverage_service.send_command service in
  Util.info "sending to %s with command:" name;
  Util.info "%s" command;
  if not dry_run then begin
    let exit_code = Sys.command command in
    let report = Coverage_service.report_filename service in
    if Sys.file_exists report then begin
      Util.info "deleting '%s'" report;
      Sys.remove report
    end;
    exit exit_code
  end

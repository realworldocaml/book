open Odoc_compat
open Printf

(* Utils *)

let (//) = Filename.concat

let command label =
  Printf.ksprintf (fun s ->
    let exit_code = Sys.command s in
    if exit_code <> 0 then
      Alcotest.failf "'%s' exited with %i" label exit_code)

(* Filename.extension is only available on 4.04. *)
module Filename = struct
  include Filename

  let extension filename =
    let dot_index = String.rindex filename '.' in
    String.sub filename dot_index (String.length filename - dot_index)
end


(* Testing environment *)

module Env = struct
  let package = "test_package"
  let odoc = "../../src/odoc/bin/main.exe"

  let path ?(from_root = false) = function
    | `scratch when from_root -> "_build/default/test/html/_scratch"
    | `scratch -> "_scratch"
    | `expect when from_root -> "test/html/expect"
    | `expect -> "expect"
    | `cases when from_root -> "test/html/cases"
    | `cases -> "cases"

  let running_in_travis_tidy_row =
    match Sys.getenv "TRAVIS", Sys.getenv "TIDY" with
    | "true", "YES" -> true
    | _ -> false
    | exception Not_found -> false

  let init () = begin
    if running_in_travis_tidy_row && not Tidy.is_present_in_path then begin
      Alcotest.failf "Could not find `tidy` in $PATH in a CI environment"
    end;

    Unix.mkdir (path `scratch) 0o755
  end
end


(* Test case type and helpers *)

(* A test case is a description of an input source file with a specific set of
   options to be tested. Each test case results in a unique generated output to
   be compared with an actually produced one.

   All paths defined in this module are relative to the build directory. *)
module Case = struct
  type t = {
    name: string;
    kind: [ `mli | `mld | `ml ];
    theme_uri: string option;
    syntax: [ `ml | `re ];
    outputs: string list;
  }

  let make ?theme_uri ?(syntax = `ml) (input, outputs) =
    let name = Filename.chop_extension input in
    let kind =
      match Filename.extension input with
      | ".mli" -> `mli
      | ".mld" -> `mld
      | ".ml" -> `ml
      | _ ->
        invalid_arg (sprintf "Expected mli, mld, or ml files, got %s" input)
    in
    { name; kind; theme_uri; syntax; outputs }

  let name case = case.name
  let kind case = case.kind
  let theme_uri case = case.theme_uri

  let string_of_syntax = function
    | `re -> "re"
    | `ml -> "ml"

  (* The package name is enriched with test case options. *)
  let package case =
    let opts = [string_of_syntax case.syntax] in
    let opts =
      match case.theme_uri with
      | Some _ -> "custom_theme" :: opts
      | None -> opts in
    let opts = String.concat "," (List.sort compare opts) in
    Env.package ^ "+" ^ opts

  let cmi_file case  = Env.path `scratch // (case.name ^ ".cmi")
  let cmti_file case = Env.path `scratch // (case.name ^ ".cmti")
  let cmo_file case = Env.path `scratch // (case.name ^ ".cmo")
  let cmt_file case = Env.path `scratch // (case.name ^ ".cmt")

  let odoc_file case =
    match case.kind with
    | `mli | `ml -> Env.path `scratch // (case.name ^ ".odoc")
    | `mld -> Env.path `scratch // ("page-" ^ case.name ^ ".odoc")

  let source_file case =
    match case.kind with
    | `mli -> Env.path `cases // case.name ^ ".mli"
    | `mld -> Env.path `cases // case.name ^ ".mld"
    | `ml -> Env.path `cases // case.name ^ ".ml"

  let outputs case =
    List.map (fun o -> package case // o) case.outputs
end


let pretty_print_html_in_place html_file =
  let temporary_pretty_printed_file = html_file ^ ".pretty" in
  let html_stream, close_html_file = Markup.file html_file in

  html_stream
  |> Markup.parse_html
  |> Markup.signals
  |> Markup.pretty_print
  |> Markup.write_html
  |> Markup.to_file temporary_pretty_printed_file;

  close_html_file ();

  Sys.rename temporary_pretty_printed_file html_file


let generate_html case =
  let theme_uri_option =
    match Case.theme_uri case with
    | Some theme_uri -> "--theme-uri=" ^ theme_uri
    | None -> ""
  in
  match Case.kind case with
  | `mli ->
    command "ocamlfind c" "ocamlfind c -bin-annot -o %s -c %s"
      (Case.cmi_file case) (Case.source_file case);

    command "odoc compile" "%s compile --package=%s %s"
      Env.odoc (Case.package case) (Case.cmti_file case);

    command "odoc html" "%s html %s --syntax=%s --output-dir=%s %s"
      Env.odoc theme_uri_option (Case.string_of_syntax case.syntax)
      (Env.path `scratch) (Case.odoc_file case)

  | `mld ->
    command "odoc compile" "%s compile --package=%s -o %s %s"
      Env.odoc
      (Case.package case) (Case.odoc_file case) (Case.source_file case);

    command "odoc html" "%s html %s --output-dir=%s %s"
      Env.odoc theme_uri_option (Env.path `scratch) (Case.odoc_file case)

  | `ml ->
    command "ocamlfind c" "ocamlfind c -bin-annot -o %s -c %s"
      (Case.cmo_file case) (Case.source_file case);

    command "odoc compile" "%s compile --package=%s %s"
      Env.odoc (Case.package case) (Case.cmt_file case);

    command "odoc html" "%s html %s --syntax=%s --output-dir=%s %s"
      Env.odoc theme_uri_option (Case.string_of_syntax case.syntax)
      (Env.path `scratch) (Case.odoc_file case)

let diff =
  (* Alcotest will run all tests. We need to know when something fails for the
     first time to stop diffing and generating promotion files. *)
  let already_failed = ref false in
  fun output ->
    let actual_file   = Env.path `scratch // output in
    let expected_file = Env.path `expect  // output in
    let cmd = sprintf "diff -u %s %s" expected_file actual_file in
    match Sys.command cmd with
    | 0 -> ()

    | 1 when !already_failed ->
      (* Don't run diff for other failing tests as only one at time is shown. *)
      Alcotest.fail "generated HTML should match expected"

    | 1 ->
      (* If the diff command exits with 1, the two HTML files are different.
         diff has already written its output to STDOUT.

         Also provide the command for overwriting the expected output with the
         actual output, in case it is the actual output that is correct.
         The paths are defined relative to the project's root. *)
      let root_actual_file   = Env.path `scratch ~from_root:true // output in
      let root_expected_file = Env.path `expect  ~from_root:true // output in
      let write_file filename data =
        Markup.string data |> Markup.to_file filename in
      write_file Env.(path `scratch // "actual") root_actual_file;
      write_file Env.(path `scratch // "expected") root_expected_file;

      prerr_endline "\nTo promote the actual output to expected, run:";
      Printf.eprintf "cp `cat %s` `cat %s` && make test\n\n"
        Env.(path ~from_root:true `scratch // "actual")
        Env.(path ~from_root:true `scratch // "expected");

      already_failed := true;
      Alcotest.fail "generated HTML should match expected"

    | exit_code ->
      Alcotest.failf "'diff' exited with %i" exit_code


(* Actual Tests *)

let output_support_files =
  let run () =
    command "odoc support-files" "%s support-files --output-dir %s"
      Env.odoc (Env.path `scratch)
  in
  "support-files", `Slow, run


let make_test_case ?theme_uri ?syntax case =
  let case = Case.make ?theme_uri ?syntax case in
  let run () =
    (* Compile the source file and generate HTML. *)
    generate_html case;

    List.iter begin fun output ->
      let actual_file = Env.path `scratch // output in

      (* Pretty-print output HTML for better diffing. *)
      pretty_print_html_in_place actual_file;

      (* Run HTML validation on output files. *)
      if Tidy.is_present_in_path then begin
        let issues = Tidy.validate actual_file in
        if issues <> [] then begin
          List.iter prerr_endline issues;
          Alcotest.fail "Tidy validation error"
        end
      end;

      (* Diff the actual outputs with the expected outputs. *)
      diff output
    end
      (Case.outputs case)
  in
  Case.name case, `Slow, run

let source_files_all = [
  ("val.mli", ["Val/index.html"]);
  ("markup.mli", ["Markup/index.html"]);
  ("section.mli", ["Section/index.html"]);
  ("module.mli", ["Module/index.html"]);
  ("interlude.mli", ["Interlude/index.html"]);
  ("include.mli", ["Include/index.html"]);
  ("include2.ml", ["Include2/index.html"]);
  ("mld.mld", ["mld.html"]);
  ("nested.mli", [
      "Nested/index.html";
      "Nested/F/index.html";
      "Nested/F/argument-1-Arg1/index.html";
      "Nested/F/argument-2-Arg2/index.html";
      "Nested/X/index.html";
      "Nested/class-z/index.html";
      "Nested/class-inherits/index.html";
      "Nested/module-type-Y/index.html";
    ]);
  ("type.mli", ["Type/index.html"]);
  ("external.mli", ["External/index.html"]);
  ("functor.mli", ["Functor/index.html"]);
  ("class.mli", ["Class/index.html"]);
  ("stop.mli", ["Stop/index.html"]);
  ("bugs.ml", ["Bugs/index.html"]);
  ("alias.ml", [
      "Alias/index.html";
      "Alias/X/index.html";
    ])
]

let source_files_post408 =
  [ ("recent.mli", ["Recent/index.html"; "Recent/X/index.html"])
  ; ("recent_impl.ml", ["Recent_impl/index.html"]) ]

let source_files_pre410 =
  [ ("bugs_pre_410.ml", ["Bugs_pre_410/index.html"]) ]

let source_files =
  let cur = Astring.String.cuts ~sep:"." (Sys.ocaml_version) |> List.map (fun i -> try Some (int_of_string i) with _ -> None) in
  match cur with
  | Some major :: Some minor :: _ ->
    List.concat
      [ (if major=4 && minor<10 then source_files_pre410 else [])
      ; (if major=4 && minor>8 then source_files_post408 else [])
      ; source_files_all ]
  | _ -> source_files_all

let () =
  Env.init ();

  Alcotest.run "html" [
    "support_files", [output_support_files];
    "html_ml", List.map (make_test_case ~syntax:`ml) source_files;
    "html_re", List.map (make_test_case ~syntax:`re) source_files;
    "custom_theme", [
      make_test_case ~theme_uri:"/a/b/c" ("module.mli", ["Module/index.html"]);
      make_test_case ~theme_uri:"https://foo.com/a/b/c/" ("val.mli", ["Val/index.html"]);
      make_test_case ~theme_uri:"../b/c" ("include.mli", ["Include/index.html"]);
      make_test_case ~theme_uri:"b/c" ("section.mli", ["Section/index.html"]);
    ];
  ]

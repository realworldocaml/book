(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



open OUnit2

let directory = "_scratch"
let preserve_directory = "_preserve"

let dune_build_directory =
  let rec scan path =
    if Filename.basename path = "_build" then
      path
    else
      scan (Filename.dirname path)
  in
  scan (Sys.getcwd ())

let test_context = ref None

let read_file name =
  let buffer = Buffer.create 4096 in
  let channel = open_in name in

  try
    let rec read () =
      try input_char channel |> Buffer.add_char buffer; read ()
      with End_of_file -> ()
    in
    read ();
    close_in channel;

    Buffer.contents buffer

  with exn ->
    close_in_noerr channel;
    raise exn

let command_failed ?status command =
  match status with
  | None ->
    Printf.sprintf "'%s' did not exit" command |> assert_failure
  | Some v ->
    let header = Printf.sprintf "'%s' failed with status %i" command v in
    let full_message =
      try
        let output = read_file "output" in
        Printf.sprintf "%s\nOutput:\n\n%s" header output
      with Sys_error _ ->
        header
    in
    assert_failure full_message

let run_int command =
  begin
    match !test_context with
    | None -> ()
    | Some context -> logf context `Info "Running '%s'" command
  end;

  match Unix.system command with
  | Unix.WEXITED v -> v
  | _ -> command_failed command

let run command =
  let v = run_int command in
  if v <> 0 then command_failed command ~status:v

let run_bool command = run_int command = 0

let test_directory = ref directory

let () =
  Random.self_init ()

let with_directory context test_name f =
  let ms =
    Unix.gettimeofday () |> modf |> fst |> ( *. ) 1000. |> int_of_float in
  let directory =
    Printf.sprintf "%s.%s.%i.%i.%i"
      directory test_name (Unix.getpid ()) (Random.int 1000000000) ms
  in
  test_directory := directory;

  Unix.mkdir directory 0o755;

  let old_wd = Sys.getcwd () in
  let new_wd = Filename.concat old_wd directory in
  Sys.chdir new_wd;

  test_context := Some context;

  let restore () =
    test_context := None;
    Sys.chdir old_wd
  in

  logf context `Info "In directory '%s'" new_wd;

  try f (); restore ()
  with exn -> restore (); raise exn

let with_bisect_args arguments =
  let ppxopt =
    if String.trim arguments = "" then ""
    else "-ppxopt 'bisect_ppx," ^ arguments ^ "'"
  in

  "-package bisect_ppx " ^ ppxopt

let with_bisect () = with_bisect_args ""

let test name f =
  name >:: fun context -> with_directory context name f

let have_package package =
  run_bool ("ocamlfind query " ^ package ^ "> /dev/null 2> /dev/null")

let ocamlc_version () =
  Scanf.sscanf Sys.ocaml_version "%u.%u%[.]%[0-9]"
    (fun major minor _periods patchlevel ->
        major, minor, try Some (int_of_string patchlevel) with _ -> None)

let ocamlc_403_or_more () =
  ocamlc_version () >= (4,3,None)

let ocamlc_404_or_more () =
  ocamlc_version () >= (4,4,None)

let ocamlc_408_or_more () =
  ocamlc_version () >= (4,8,None)

let if_package package =
  skip_if (not @@ have_package package) (package ^ " not installed")

let compile ?(r = "") arguments source =
  let source_copy = Filename.basename source in

  let intermediate = Filename.dirname source = directory in
  begin
    if not intermediate then
      let source_actual = Filename.concat Filename.parent_dir_name source in
      run ("cp " ^ source_actual ^ " " ^ source_copy)
  end;

  Printf.sprintf
    "OCAMLPATH=%s:$OCAMLPATH %s ocamlfind c -linkpkg %s %s %s"
    (Filename.concat dune_build_directory "install/default/lib")
    "OCAML_COLOR=never OCAML_ERROR_STYLE=short"
    arguments source_copy r
  |> run

let report
    ?(env = []) ?(f = "bisect*.coverage") ?(r = "> /dev/null") arguments =

  let clear_env =
    "unset TRAVIS && export TRAVIS && " ^
    "unset TRAVIS_JOB_ID && export TRAVIS_JOB_ID"
  in

  let env =
    env
    |> List.map (fun (variable, value) ->
      Printf.sprintf "export %s=%s" variable value)
    |> fun l -> l @ [""]
    |> String.concat " && "
  in

  Printf.sprintf
    "%s && %s%s %s %s %s"
    clear_env
    env
    (Filename.concat
      dune_build_directory "install/default/bin/bisect-ppx-report")
    arguments f r
  |> run

let preserve file destination =
  let destination =
    destination
    |> Filename.concat preserve_directory
    |> Filename.concat Filename.parent_dir_name
  in

  run ("mkdir -p " ^ (Filename.dirname destination));
  run ("cp " ^ file ^ " " ^ destination)

let diff ?preserve_as reference =
  let preserve_as =
    match preserve_as with
    | None -> reference
    | Some preserve_as -> preserve_as
  in

  let reference_actual = Filename.concat Filename.parent_dir_name reference in
  let command =
    Printf.sprintf
      "diff -au --label %s --label %s %s output 2> /dev/null"
      preserve_as "'actual output'" reference_actual
  in

  let status = run_int (command ^ " > /dev/null") in
  match status with
  | 0 -> ()
  | 1 ->
    preserve "output" preserve_as;
    run_int (command ^ " > delta") |> ignore;
    let delta = read_file "delta" in
    Printf.sprintf "Difference against '%s':\n\n%s" preserve_as delta
    |> assert_failure
  | 2 ->
    preserve "output" preserve_as;
    command_failed command ~status
  | _ ->
    command_failed command ~status

let normalize_source source normalized =
  let source = read_file source in
  let normalized_file = open_out normalized in
  try
    let lexbuf = Lexing.from_string source in
    let structure = Parse.implementation lexbuf in

    let formatter = Format.formatter_of_out_channel normalized_file in
    Pprintast.structure formatter structure;
    Format.pp_print_newline formatter ();

    close_out_noerr normalized_file

  with e ->
    close_out_noerr normalized_file;
    raise e

let diff_ast reference =
  let reference_actual = Filename.concat Filename.parent_dir_name reference in
  normalize_source reference_actual "_dsource";
  let dsource = Filename.concat !test_directory "_dsource" in
  diff ~preserve_as:reference dsource

let compile_compare cflags directory =
  let directory = Filename.concat "fixtures" directory in
  let tests =
    (* Get a list of all files in the given directory, whose name does not start
       with "test_", does end in ".ml", but not ".reference.ml". *)
    let files_in_directory = Sys.readdir directory |> Array.to_list in
    let ml_file_names =
      List.filter (fun f -> Filename.check_suffix f ".ml") files_in_directory in
    let source_file_names =
      ml_file_names
      |> List.filter (fun f ->
        let f = Filename.chop_suffix f ".ml" in
        not (Filename.check_suffix f ".reference"))
      |> List.filter (fun f ->
        let prefix = "test_" in
        let length = String.length prefix in
        String.length f < length || String.sub f 0 length <> prefix)
    in

    (* Create a test for each source file. *)
    source_file_names
    |> List.map begin fun f ->
      let source = Filename.concat directory f in
      let title = Filename.chop_suffix f ".ml" in
      let reference = Filename.concat directory (title ^ ".reference.ml") in
      test title (fun () ->
        if Filename.check_suffix title "_403" then
          skip_if (not (ocamlc_403_or_more ())) "requires OCaml 4.03 or more";
        if Filename.check_suffix title "_404" then
          skip_if (not (ocamlc_404_or_more ())) "requires OCaml 4.04 or more";
        if Filename.check_suffix title "_408" then
          skip_if (not (ocamlc_408_or_more ())) "requires OCaml 4.08 or more";
        compile ((cflags ()) ^ " -c -w -A -dsource") source ~r:"2> output";
        diff_ast reference)
    end
  in

  directory >::: tests

let test_directory () =
  !test_directory

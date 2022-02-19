(* Copied from Filename (stdlib) for pre-4.04 compatibility *)
let chop_extension name =
  let is_dir_sep s i =
    match Sys.os_type with
    | "Unix" -> s.[i] = '/'
    | "Win32" | "Cygwin" ->
        let c = s.[i] in
        c = '/' || c = '\\' || c = ':'
    | _ -> assert false
  in
  let rec search_dot i =
    if i < 0 || is_dir_sep name i then invalid_arg "Filename.chop_extension"
    else if name.[i] = '.' then String.sub name 0 i
    else search_dot (i - 1)
  in
  search_dot (String.length name - 1)

(* Read a file-path to a list of strings corresponding to lines in the file. *)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    assert false
  with End_of_file ->
    close_in chan;
    List.rev !lines

(* A test executable [foo.ml] may require certain options to be passed as
   specified in a [foo.opts] file. *)
let options_of_test_file file =
  let options_file = chop_extension file ^ ".opts" in
  if not (Sys.file_exists options_file) then [] else read_file options_file

let global_stanza ~libraries ~js filenames =
  let bases = List.map chop_extension filenames in
  let libraries = List.map (( ^ ) " ") libraries in
  let pp_sexp_list = Fmt.(list ~sep:(const string "\n   ")) in
  Fmt.pr
    {|(executables
 (names
   %a
 )
 (libraries alcotest alcotest.stdlib_ext%a)
 %s
 (modules
   %a
 )
)
|}
    (pp_sexp_list Fmt.string) bases
    Fmt.(list string)
    libraries
    (if js then "(modes native js)" else "(modes native)")
    (pp_sexp_list Fmt.string) bases

let example_rule_stanza ~js ~expect_failure filename =
  let with_suffix x = if js then x ^ "-js" else x in
  let base = chop_extension filename in
  let options = options_of_test_file filename |> List.map (( ^ ) " ") in
  let accepted_exit_codes =
    Fmt.str "(or %s %d)"
      (if expect_failure then
       (* 1 = failing test, 2 = failed assertion outside runner *)
       "1 2"
      else "0")
      Cmdliner.Term.exit_status_internal_error
  in
  (* Run Alcotest to get *.actual, then pass through the strip_randomness
     sanitiser to get *.processed. *)
  Fmt.pr
    {|
(rule
 (target %s.actual)
 (action
  (with-outputs-to %%{target}
   (with-accepted-exit-codes %s
    (run %s%a)))))
|}
    (with_suffix base) accepted_exit_codes
    (if js then Printf.sprintf "node %%{dep:%s.bc.js}" base
    else Printf.sprintf "%%{dep:%s.exe}" base)
    Fmt.(list string)
    options;

  Fmt.pr
    {|
(rule
 (target %s.processed)
 (action
  (with-outputs-to %%{target}
   (run ../../strip_randomness.exe %%{dep:%s.actual}))))
|}
    (with_suffix base) (with_suffix base)

let example_alias_stanza ~js ~package filename =
  let with_suffix x = if js then x ^ "-js" else x in
  let base = chop_extension filename in
  Fmt.pr
    {|
(rule
 (alias %s)
 (package %s)
 (action
   (diff %s.expected %s.processed)))
|}
    (with_suffix "runtest") package (with_suffix base) (with_suffix base)

let is_example filename = Filename.check_suffix filename ".ml"

let main package expect_failure libraries js =
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_example
  |> function
  | [] -> () (* no tests to execute *)
  | tests ->
      global_stanza ~libraries ~js tests;
      List.iter
        (fun test ->
          example_rule_stanza ~js:false ~expect_failure test;
          example_alias_stanza ~js:false ~package test;
          if js then (
            example_rule_stanza ~js:true ~expect_failure test;
            example_alias_stanza ~js:true ~package test))
        tests

open Cmdliner

let package =
  let doc =
    Arg.info ~doc:"Package with which to associate the executables"
      [ "package" ]
  in
  Arg.(required & opt (some string) None & doc)

let libraries =
  let doc =
    Arg.info ~doc:"Additional libraries to make available to the executable"
      [ "libraries" ]
  in
  Arg.(value & opt (list string) [] & doc)

let js =
  let doc = Arg.info ~doc:"Test in javascript" [ "js" ] in
  Arg.(value & flag doc)

let expect_failure =
  let doc =
    Arg.info ~doc:"Negate the return status of the tests" [ "expect-failure" ]
  in
  Arg.(value & flag doc)

let term =
  Term.
    ( const main $ package $ expect_failure $ libraries $ js,
      info ~version:"1.5.0" "gen_dune_rules" )

let () = Term.(exit @@ eval term)

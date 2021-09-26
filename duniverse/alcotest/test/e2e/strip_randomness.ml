let ( ^^ ) a b = Re.seq [ a; b ]

let standardise_filesep =
  let re = Re.compile (Re.str Filename.dir_sep) in
  Re.replace_string ~all:true re ~by:"/"

let build_context_replace =
  let open Re in
  let lterm, rterm =
    (* Contexts in which directories are printed (tests, manpage output
       etc.). *)
    ( group (alt [ char '`'; str "(absent=" ]),
      group (alt [ char '\''; char ')' ]) )
  in
  let t =
    seq
      [
        lterm;
        rep any;
        str ("_build" ^ Filename.dir_sep ^ "_tests");
        opt (str Filename.dir_sep);
        group (rep (diff any (set Filename.dir_sep)))
        (* <test-dir>: May be a UUID or a suite name (symlink), depending on
           whether or not we're running on Windows *);
        group (opt (seq [ str Filename.dir_sep; rep any ]));
        rterm;
      ]
  in
  let re = compile t in
  replace ~all:true re ~f:(fun g ->
      let test_dir_opt = if Group.get g 2 = "" then "" else "<test-dir>" in
      let test_name = standardise_filesep (Group.get g 3) in
      Group.get g 1
      ^ "<build-context>/_build/_tests/"
      ^ test_dir_opt
      ^ test_name
      ^ Group.get g 4)

let uuid_replace =
  let open Re in
  let hex n = repn (alt [ rg 'A' 'F'; digit ]) n (Some n) in
  let segmented_hex ns =
    let segments = List.map (fun n -> [ char '-'; hex n ]) ns in
    List.flatten segments |> List.tl |> seq
  in
  let t = segmented_hex [ 8; 4; 4; 4; 12 ] in
  let re = compile t in
  replace_string ~all:true re ~by:"<uuid>"

let time_replace =
  let open Re in
  let float = rep1 digit ^^ char '.' ^^ rep1 digit in
  let t =
    alt
      [
        group
          (alt
             [
               (* Maybe ANSII escape, depending on [--color] *)
               opt cntrl ^^ str "Test Successful" ^^ opt cntrl ^^ str " in ";
               rep1 digit ^^ str " failure! in ";
               rep1 digit ^^ str " failures! in ";
               str "\"time\": ";
             ])
        ^^ float
        ^^ group (opt (char 's'));
      ]
  in
  let re = compile t in
  replace re ~f:(fun g -> Group.get g 1 ^ "<test-duration>" ^ Group.get g 2)

let exception_name_replace =
  let open Re in
  let t = str "Alcotest_engine__Model.Registration_error" in
  let re = compile t in
  replace_string ~all:true re ~by:"Alcotest_engine.Model.Registration_error"

(* Remove all non-deterministic output in a given Alcotest log and write
   the result to std.out *)
let () =
  let in_channel = open_in Sys.argv.(1) in
  try
    let rec loop () =
      let sanitized_line =
        input_line in_channel
        |> uuid_replace
        |> build_context_replace
        |> time_replace
        |> exception_name_replace
      in
      Printf.printf "%s\n" sanitized_line;
      loop ()
    in
    loop ()
  with End_of_file -> close_in in_channel

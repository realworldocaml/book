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
  let dir_sep = alt [ str Filename.dir_sep; char '/' ] in
  let t =
    seq
      [
        lterm;
        rep any;
        str "_build";
        dir_sep;
        str "_tests";
        opt dir_sep;
        group (rep (diff any (set "\\/")))
        (* <test-dir>: May be a UUID or a suite name (symlink), depending on
           whether or not we're running on Windows *);
        group (opt (seq [ dir_sep; rep any ]));
        rterm;
      ]
  in
  let re = compile t in
  fun s ->
    replace ~all:true re s ~f:(fun g ->
        let test_dir_opt = if Group.get g 2 = "" then "" else "<test-dir>" in
        let test_name = standardise_filesep (Group.get g 3) in
        Group.get g 1
        ^ "<build-context>/_build/_tests/"
        ^ test_dir_opt
        ^ test_name
        ^ Group.get g 4)

let uuid_replace =
  let open Re in
  let t =
    seq [ str "ID `"; repn (alt [ rg 'A' 'Z'; digit ]) 8 (Some 8); char '\'' ]
  in
  let re = compile t in
  fun s -> replace_string ~all:true re ~by:"ID `<uuid>'" s

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
  fun s ->
    replace re s ~f:(fun g -> Group.get g 1 ^ "<test-duration>" ^ Group.get g 2)

let stacktrace_replace =
  let open Re in
  let stack_trace_line verb =
    str verb
    ^^ opt (rep1 print ^^ str " in ") (* exception name *)
    ^^ str "file \""
    ^^ rep1 print
    ^^ str "\""
    ^^ opt (str " (inlined)")
    ^^ str ", line "
    ^^ rep1 digit
    ^^ str ", characters "
    ^^ rep1 digit
    ^^ str "-"
    ^^ rep1 digit
    |> compile
  in
  let raised_at = stack_trace_line "Raised at "
  and called_from = stack_trace_line "Called from " in
  fun s ->
    match execp called_from s with
    | true ->
        (* The number of "Called from ..." lines is compiler-dependent, so we
           remove them all. *)
        None
    | false -> Some (replace_string ~all:true raised_at s ~by:"<stacktrace>")

let executable_name_normalization =
  let open Re in
  let t = alt [ str ".exe"; str ".bc.js" ] in
  let re = compile t in
  replace_string ~all:true re ~by:".<ext>"

(* Remove all non-deterministic output in a given Alcotest log and write
   the result to std.out *)
let () =
  let in_channel = open_in Sys.argv.(1) in
  let ( >>| ) x f = match x with None -> None | Some x -> Some (f x) in
  let ( >>= ) x f = match x with None -> None | Some x -> f x in
  try
    let rec loop () =
      let sanitized_line =
        Some (input_line in_channel)
        >>| uuid_replace
        >>| build_context_replace
        >>| time_replace
        >>| executable_name_normalization
        >>= stacktrace_replace
      in
      (match sanitized_line with
      | Some s -> Printf.printf "%s\n" s
      | None -> ());
      loop ()
    in
    loop ()
  with End_of_file -> close_in in_channel

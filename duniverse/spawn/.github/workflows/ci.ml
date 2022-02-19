open StdLabels

let run cmd args =
  (* broken when arguments contain spaces but it's good enough for now. *)
  let cmd = String.concat " " (cmd :: args) in
  match Sys.command cmd with
  | 0 -> ()
  | n ->
    Printf.eprintf "'%s' failed with code %d" cmd n;
    exit n

let opam args = run "opam" args

let pin () =
  let packages =
    let packages = Sys.readdir "." |> Array.to_list in
    List.fold_left packages ~init:[] ~f:(fun acc fname ->
        if Filename.check_suffix fname ".opam" then
          Filename.chop_suffix fname ".opam" :: acc
        else
          acc)
  in
  List.iter packages ~f:(fun package ->
      opam [ "pin"; "add"; package ^ ".next"; "."; "--no-action" ])

let test () =
  opam [ "install"; "."; "--deps-only"; "--with-test" ];
  run "dune" [ "runtest" ]

let fmt () =
  let version_of_ocamlformat =
    let ic = open_in ".ocamlformat" in
    let v = Scanf.sscanf (input_line ic) "version=%s" (fun x -> x) in
    close_in ic;
    v
  in
  opam [ "install"; "ocamlformat." ^ version_of_ocamlformat ];
  run "dune" [ "build"; "@fmt" ]

let () =
  match Sys.argv with
  | [| _; "pin" |] -> pin ()
  | [| _; "test" |] -> test ()
  | [| _; "fmt" |] -> fmt ()
  | _ ->
    prerr_endline "Usage: ci.ml [pin | test]";
    exit 1

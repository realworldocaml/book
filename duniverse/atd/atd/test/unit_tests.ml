(*
   Entrypoint to run the unit tests from the command line.
*)

let test_suites : unit Alcotest.test list = [
  Annot.test;
  "Sort", [
    "sort", `Quick, Atd.Sort.test
  ];
  Unique_name.test;
  Doc.test;
]

let main () = Alcotest.run "atd" test_suites

let () = main ()

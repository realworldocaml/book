(*
   Entry point to the executable running the unit tests

   TODO: We don't have any test suite right now. Remove?
*)

let test_suites : unit Alcotest.test list = [
  (* Unique_name.test *)
]

let main () =
  Alcotest.run "atdts" test_suites

let () = main ()

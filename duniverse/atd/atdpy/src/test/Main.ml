(*
   Entry point to the executable running the unit tests

   TODO: the only test suite we had moved to the atd library. Remove?
*)

let test_suites : unit Alcotest.test list = [
  (* Unique_name.test *)
]

let main () =
  Alcotest.run "atdpy" test_suites

let () = main ()

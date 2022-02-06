(** Test of the "in progress" output lines printed for each test-case (which are
    only shown when standard output is a TTY). *)

module Platform (M : Alcotest_engine.Monad.S) = struct
  include Alcotest.Unix_platform (M)

  let stdout_isatty () = true
end

module Alcotest =
  Alcotest_engine.V1.Core.Make (Platform) (Alcotest_engine.Monad.Identity)

let () =
  let open Alcotest in
  let id () = () in
  run __FILE__
    [
      ( "test-a",
        [
          test_case "First test case" `Quick id;
          test_case "Second test case" `Quick id;
        ] );
      ("test-b", [ test_case "Third test case" `Quick id ]);
      ("test-c", [ test_case "Fourth test case" `Slow id ]);
    ]

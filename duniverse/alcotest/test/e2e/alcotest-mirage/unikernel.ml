module Main (C : Mirage_clock.MCLOCK) = struct
  module Alcotest = Alcotest_mirage.Make (C)

  let start () =
    let open Alcotest in
    let id () = () in
    run "suite-name"
      [
        ( "test",
          [ test_case_sync "A test case for alcotest with mirage" `Quick id ] );
      ]
end

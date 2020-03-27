module Testable = struct
  open Mdx.Syntax

  let syntax = Alcotest.testable pp equal
end

let test_infer =
  let make_test ~file ~expected () =
    let test_name = Printf.sprintf "infer: %S" file in
    let test_fun () =
      let actual = Mdx.Syntax.infer ~file in
      Alcotest.(check (option Testable.syntax)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~file:"" ~expected:None ();
    make_test ~file:"test.md" ~expected:(Some Normal) ();
    make_test ~file:"test.t" ~expected:(Some Cram) ();
    make_test ~file:"test.ml" ~expected:None ();
    make_test ~file:"test.mli" ~expected:(Some Mli) ();
    make_test ~file:"no_ext" ~expected:None ();
  ]

let suite = ("Syntax", test_infer)

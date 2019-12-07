module Testable = struct
  open Mdx.Library

  let library = Alcotest.testable pp equal
end

let test_from_string =
  let make_test ~str ~expected () =
    let test_name = Printf.sprintf "from_string: %S" str in
    let test_fun () =
      let actual = Mdx.Library.from_string str in
      Alcotest.(check (result Testable.library string)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test ~str:"some-lib" ~expected:(Ok {base_name = "some-lib"; sub_lib = None}) ()
  ; make_test ~str:"some.lib" ~expected:(Ok {base_name = "some"; sub_lib = Some "lib"}) ()
  ; make_test ~str:"" ~expected:(Error "Invalid library name: \"\"") ()
  ; make_test ~str:".lib" ~expected:(Error "Invalid library name: \".lib\"") ()
  ; make_test ~str:"some." ~expected:(Error "Invalid library name: \"some.\"") ()
  ; make_test ~str:"a.b.c" ~expected:(Error "Invalid library name: \"a.b.c\"") ()
  ]

let suite = ("Library", test_from_string)

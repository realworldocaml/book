module Testable = struct
  open Mdx.Label

  let relation = Alcotest.testable Relation.pp ( = )

  let msg =
    Alcotest.testable (fun fmt (`Msg e) -> Format.pp_print_string fmt e) ( = )

  let label = Alcotest.testable pp ( = )
end

let test_raw_parse =
  let ty = Alcotest.(pair string (option (pair Testable.relation string))) in
  let make_test ~input ~expected =
    let test_name = Printf.sprintf "raw_parse: %S" input in
    let test_fun () =
      Alcotest.check ty test_name expected (Mdx.Label.Relation.raw_parse input)
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~input:"" ~expected:("", None);
    make_test ~input:"foo=" ~expected:("foo", Some (Eq, ""));
    make_test ~input:"foo<>bar" ~expected:("foo", Some (Neq, "bar"));
    make_test ~input:"<bar" ~expected:("", Some (Lt, "bar"));
    make_test ~input:"foo<=bar" ~expected:("foo", Some (Le, "bar"));
    make_test ~input:"foo>bar" ~expected:("foo", Some (Gt, "bar"));
    make_test ~input:">=" ~expected:("", Some (Ge, ""));
    make_test ~input:"FOO_BAR=foo_bar"
      ~expected:("FOO_BAR", Some (Eq, "foo_bar"));
  ]

let test_interpret =
  let ty = Alcotest.result Testable.label Testable.msg in
  let make_test ~label ~value ~expected =
    let test_name = Printf.sprintf "interpret: %S" label in
    let test_fun () =
      Alcotest.check ty test_name expected (Mdx.Label.interpret label value)
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~label:"" ~value:None
      ~expected:(Error (`Msg "`` is not a valid label."));
    make_test ~label:"foo"
      ~value:(Some (Eq, "bar"))
      ~expected:(Error (`Msg "`foo` is not a valid label."));
    make_test ~label:"skip" ~value:None ~expected:(Ok Skip);
    make_test ~label:"skip"
      ~value:(Some (Eq, ""))
      ~expected:(Error (`Msg "Label `skip` does not allow a value."));
    make_test ~label:"version" ~value:None
      ~expected:(Error (`Msg "Label `version` requires a value."));
    make_test ~label:"non-deterministic"
      ~value:(Some (Eq, "bar"))
      ~expected:
        (Error
           (`Msg
             "\"bar\" is not a valid value for label `non-deterministic`. \
              Valid values are <none>, \"command\" and \"output\"."));
    make_test ~label:"dir"
      ~value:(Some (Lt, "bar"))
      ~expected:
        (Error (`Msg "Label `dir` requires assignment using the `=` operator."));
    make_test ~label:"set-foo"
      ~value:(Some (Eq, "bar"))
      ~expected:(Ok (Set ("foo", "bar")));
  ]

let test_of_string =
  let ty = Alcotest.(result (list Testable.label) (list Testable.msg)) in
  let make_test ~input ~expected =
    let test_name = Printf.sprintf "of_string: %S" input in
    let test_fun () =
      Alcotest.check ty test_name expected (Mdx.Label.of_string input)
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~input:"" ~expected:(Ok []);
    make_test ~input:","
      ~expected:
        (Error
           [ `Msg "`` is not a valid label."; `Msg "`` is not a valid label." ]);
    make_test ~input:"skip,"
      ~expected:(Error [ `Msg "`` is not a valid label." ]);
    make_test ~input:"skip,file=foo.ml" ~expected:(Ok [ Skip; File "foo.ml" ]);
  ]

let suite = ("Label", test_raw_parse @ test_interpret @ test_of_string)

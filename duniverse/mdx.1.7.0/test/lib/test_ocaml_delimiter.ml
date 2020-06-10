open Mdx.Ocaml_delimiter
open Result

let test_parse =
  let make_test ~line ~expected =
    let test_name = Printf.sprintf "parse: %S" line in
    let test_fun () =
      let actual = parse line in
      Alcotest.(check (result (option Testable.ocaml_delimiter) Testable.msg))
        test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~line:"" ~expected:(Ok None);
    make_test ~line:"foo" ~expected:(Ok None);
    make_test ~line:"    (* $MDX part-begin=bar *)    "
      ~expected:
        (Ok (Some (Part_begin (Cmt, { indent = "    "; payload = "bar" }))));
    (* Unclosed comments are caught by syntax highlighting, it is okay to
       silently ignore them and not print a warning here. *)
    make_test ~line:"(* $MDX part-begin=bar     " ~expected:(Ok None);
    make_test ~line:"   (* $MDX part-end   *)   " ~expected:(Ok (Some Part_end));
    make_test ~line:"    [@@@part \"foobar\"]    "
      ~expected:
        (Ok (Some (Part_begin (Attr, { indent = "    "; payload = "foobar" }))));
    make_test ~line:"(* $MDX foo *)"
      ~expected:
        (Error (`Msg "'(* $MDX foo *)' is not a valid ocaml delimiter for mdx."));
    make_test ~line:"[@@@foo \"bar\"]" ~expected:(Ok None);
    make_test ~line:"(* $MDX part-end=foo *)"
      ~expected:
        (Error
           (`Msg
             "'part-end' delimiter does not accept a value. Please write '(* \
              $MDX part-end *)' instead."));
  ]

let suite = ("Ocaml_delimiter", test_parse)

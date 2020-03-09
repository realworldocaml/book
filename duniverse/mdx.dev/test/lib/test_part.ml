open Mdx.Part
open Result

module Testable = struct
  let parse_parts_part_decl =
    let open Parse_parts in
    let equal = ( = ) in
    let pp fmt = function
      | Normal s -> Fmt.pf fmt "Normal: %S" s
      | Compat_attr (n, sep_indent) ->
          Fmt.pf fmt "Compat_attr (%S, %S)" n sep_indent
      | Part_begin (n, sep_indent) ->
          Fmt.pf fmt "Part_begin (%S, %S)" n sep_indent
      | Part_end -> Fmt.pf fmt "Part_end"
      | File_end -> Fmt.pf fmt "File_end"
    in
    Alcotest.testable pp equal
end

let test_of_line =
  let make_test ~line ~expected () =
    let test_name =
      Printf.sprintf "parse_line: %S"
        (match line with Ok s -> s | Error `End_of_file -> "EOF")
    in
    let test_fun () =
      let actual = Parse_parts.parse_line line in
      Alcotest.(check Testable.parse_parts_part_decl) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~line:(Error `End_of_file) ~expected:File_end ();
    make_test ~line:(Ok "foo") ~expected:(Normal "foo") ();
    make_test ~line:(Ok "    (* $MDX part-begin=bar *)    ")
      ~expected:(Part_begin ("bar", "    "))
      ();
    make_test ~line:(Ok "(* $MDX part-begin=bar     ")
      ~expected:(Normal "(* $MDX part-begin=bar     ") ();
    make_test ~line:(Ok "   (* $MDX part-end   *)   ") ~expected:Part_end ();
    make_test ~line:(Ok "    [@@@part \"foobar\"]    ")
      ~expected:(Compat_attr ("foobar", "    "))
      ();
  ]

let suite = ("Parts", test_of_line)

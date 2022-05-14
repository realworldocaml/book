(*
   Unit tests for Atd.Doc
*)

open Printf

let normalize s =
  s
  |> Atd.Doc.parse_text Atd.Ast.dummy_loc
  |> Atd.Doc.print_text

let test_parser_and_printer =
  [
    "", "";
    "a", "a";
    "a b", "a b";
    "a\nb", "a b";
    "a  b", "a b";
    "a \n b", "a b";
    "a \n\n b", "a\n\nb";
    "  a  ", "a";
    "\n\na\n\n", "a";
    "{{}}", "";
    "{{a}}", "{{a}}";
    "{{  a  b\n\nc\n }}", "{{a b c}}";
    "{{ {a} }}", "{{ {a} }}";
    "{{{}}}", "";
    "{{{a}}}", "{{{\na\n}}}";
    "{{{ }}}", "{{{\n \n}}}";
    "{{{ {{a}} }}}", "{{{\n {{a}} \n}}}";
    "{{{ a\n\n  b }}}", "{{{\n a\n\n  b \n}}}";
  ]
  |> List.map (fun (input, expected_output) ->
    let name = sprintf "normalize %S" input in
    name, `Quick, (fun () ->
      let output = normalize input in
      Alcotest.(check string) "equal" expected_output output
    )
  )

let test = "Doc", test_parser_and_printer

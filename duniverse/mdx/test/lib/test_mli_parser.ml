open Mdx.Util.Result.Infix

let mli =
  {|
(** This is a doc comment with some code blocks in it:

    {[
      # List.map (fun x -> x * x) [(1 + 9); 2; 3]
      - : int list = [100; 4; 9]
    ]}

    {[List.map (fun x -> x * x) [1; 2; 3]]}

    {@ocaml [
      # List.map (fun x -> x * x) [(1 + 9); 2; 3]
      - : int list = [100; 4; 9]
      # List.map (fun x -> x * x) [1; 2; 3]
      - : int list = [1; 4; 9]
    ]}
*)
val foo : string

(** {@ocaml [1 + 1 = 3]} *)
val bar : string
|}

let header =
  {|(** This doc comment with a header should get parsed

    {@ocaml [
      # 1 + 1
      - : int = 2
    ]}
*)
|}

let label =
  {|(** This doc comment with a label should get parsed

    {@ocaml skip [
      # 1 + 1
      - : int = 2
    ]}
*)
|}

let test_parse_mli =
  let make_test ~test_name ~mli ~expected () =
    let test_fun () =
      let buffer = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buffer in
      let actual =
        Mdx.Mli_parser.parse_mli mli >>| fun lines ->
        Mdx.dump fmt lines;
        Buffer.contents buffer
      in
      Alcotest.(check (result string Testable.msg)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~test_name:"mli" ~mli
      ~expected:
        (Ok
           {x|[Text "\n(** This is a doc comment with some code blocks in it:\n\n    ";
 Text "{[";
 Block {loc: File "_none_", lines 4-7; section: None; labels: [];
        header: Some ocaml;
        contents: ["# List.map (fun x -> x * x) [(1 + 9); 2; 3]";
                   "- : int list = [100; 4; 9]"];
        value: Toplevel};
 Text "    ]}"; Text "\n\n    "; Text "{[";
 Block {loc: File "_none_", line 9; section: None; labels: [];
        header: Some ocaml;
        contents: ["List.map (fun x -> x * x) [1; 2; 3]"]; value: OCaml};
 Text "]}"; Text "\n\n    "; Text "{@"; Text "ocaml"; Text "[";
 Block {loc: File "_none_", lines 11-16; section: None; labels: [];
        header: Some ocaml;
        contents: ["# List.map (fun x -> x * x) [(1 + 9); 2; 3]";
                   "- : int list = [100; 4; 9]";
                   "# List.map (fun x -> x * x) [1; 2; 3]";
                   "- : int list = [1; 4; 9]"];
        value: Toplevel};
 Text "    ]}"; Text "\n*)\nval foo : string\n\n(** "; Text "{@";
 Text "ocaml"; Text "[";
 Block {loc: File "_none_", line 20; section: None; labels: [];
        header: Some ocaml; contents: ["1 + 1 = 3"]; value: OCaml};
 Text "]}";|x})
      ();
    make_test ~test_name:"header" ~mli:header
      ~expected:
        (Ok
           {x|[Text "(** This doc comment with a header should get parsed\n\n    ";
 Text "{@"; Text "ocaml"; Text "[";
 Block {loc: File "_none_", lines 3-6; section: None; labels: [];
        header: Some ocaml; contents: ["# 1 + 1"; "- : int = 2"];
        value: Toplevel};
 Text "    ]}";|x})
      ();
    make_test ~test_name:"label" ~mli:label
      ~expected:
        (Ok
           {x|[Text "(** This doc comment with a label should get parsed\n\n    ";
 Text "{@"; Text "ocaml"; Text " "; Text "skip"; Text "[";
 Block {loc: File "_none_", lines 3-6; section: None; labels: [skip];
        header: Some ocaml; contents: ["# 1 + 1"; "- : int = 2"];
        value: Toplevel};
 Text "    ]}";|x})
      ();
  ]

let suite = ("Mli_parser", test_parse_mli)

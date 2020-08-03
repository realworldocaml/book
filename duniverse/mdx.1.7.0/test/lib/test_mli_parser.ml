open Mdx.Util.Result.Infix

let mli =
  {|
(** This is a doc comment with some code blocks in it:

    {[
      # List.map (fun x -> x * x) [(1 + 9); 2; 3]
      - : int list = [100; 4; 9]
    ]}

    {[List.map (fun x -> x * x) [1; 2; 3]]}

    {[
      # List.map (fun x -> x * x) [(1 + 9); 2; 3]
      - : int list = [100; 4; 9]
      # List.map (fun x -> x * x) [1; 2; 3]
      - : int list = [1; 4; 9]
    ]}
*)
val foo : string

(** {[1 + 1 = 3]} *)
val bar : string
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
 Block {file: ; line: 4; column: 4; section: None; labels: [];
        header: Some ocaml;
                contents: ["# List.map (fun x -> x * x) [(1 + 9); 2; 3]";
                           "- : int list = [100; 4; 9]"];
        value: Toplevel};
 Text "    ]}"; Text "\n\n    "; Text "{[";
 Block {file: ; line: 9; column: 4; section: None; labels: [];
        header: Some ocaml;
                contents: ["List.map (fun x -> x * x) [1; 2; 3]"];
        value: OCaml};
 Text "]}"; Text "\n\n    "; Text "{[";
 Block {file: ; line: 11; column: 4; section: None; labels: [];
        header: Some ocaml;
                contents: ["# List.map (fun x -> x * x) [(1 + 9); 2; 3]";
                           "- : int list = [100; 4; 9]";
                           "# List.map (fun x -> x * x) [1; 2; 3]";
                           "- : int list = [1; 4; 9]"];
        value: Toplevel};
 Text "    ]}"; Text "\n*)\nval foo : string\n\n(** "; Text "{[";
 Block {file: ; line: 20; column: 4; section: None; labels: [];
        header: Some ocaml;
                contents: ["1 + 1 = 3"]; value: OCaml};
 Text "]}";|x})
      ();
  ]

let suite = ("Mli_parser", test_parse_mli)

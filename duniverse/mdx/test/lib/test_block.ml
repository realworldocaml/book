let test_infer_from_file =
  let make_test ~file ~expected =
    let test_name = Printf.sprintf "Header.infer_from_file: %S" file in
    let test_fun () =
      let actual = Mdx.Block.Header.infer_from_file file in
      Alcotest.(check (option Testable.header)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~file:"" ~expected:None;
    make_test ~file:"foo" ~expected:None;
    make_test ~file:"foo.bar" ~expected:None;
    make_test ~file:"dune" ~expected:(Some (Other "scheme"));
    make_test ~file:"dune-project" ~expected:(Some (Other "scheme"));
    make_test ~file:"foo.sh" ~expected:(Some (Shell `Sh));
    make_test ~file:"foo/foo/foo.ml" ~expected:(Some OCaml);
  ]

let test_mk =
  let make_test ~name ~labels ~header ~contents ~expected =
    let test_name = Printf.sprintf "mk: %S" name in
    let test_fun () =
      let actual =
        Mdx.Block.mk ~loc:Location.none ~section:None ~labels
          ~legacy_labels:false ~header ~contents ~errors:[]
      in
      Alcotest.(check (result Testable.block Testable.msg))
        test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"invalid ocaml" ~labels:[ Block_kind OCaml ]
      ~header:(Some OCaml) ~contents:[ "# let x = 2;;" ]
      ~expected:(Error (`Msg "toplevel syntax is not allowed in OCaml blocks."));
    make_test ~name:"invalid toplevel" ~labels:[ Block_kind Toplevel ]
      ~header:(Some OCaml) ~contents:[ "let x = 2;;" ]
      ~expected:(Error (`Msg "invalid toplevel syntax in toplevel blocks."));
  ]

let suite = ("Block", test_infer_from_file @ test_mk)

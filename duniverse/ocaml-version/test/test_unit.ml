let test_configure_options =
  let test name version option ~expected =
    ( name,
      `Quick,
      fun () ->
        let got =
          Ocaml_version.Configure_options.to_configure_flag version option
        in
        Alcotest.check Alcotest.string __LOC__ expected got )
  in
  let v3_12 = Ocaml_version.of_string_exn "3.12.1" in
  [
    test "FP on last 4.07" Ocaml_version.Releases.v4_07 `Frame_pointer
      ~expected:"-with-frame-pointer";
    test "FP on first 4.08" Ocaml_version.Releases.v4_08_0 `Frame_pointer
      ~expected:"--enable-frame-pointers";
    test "FP on 3.12" v3_12 `Frame_pointer ~expected:"-with-frame-pointer";
  ]

let test_compiler_variants =
  let test ?(expect_exists=true) name arch version ~expected =
    ( name,
      `Quick,
      fun () ->
        let got = Ocaml_version.compiler_variants arch version in
        let all_ok = expected |> List.for_all (
          fun expected_extra ->
            let exists = got |> List.exists (fun v -> Ocaml_version.extra v = Some expected_extra) in
            exists = expect_exists
        ) in
        Alcotest.(check bool __LOC__ all_ok true) )
  in
  Ocaml_version.Releases.([
    test "Multicore on 4.12 x86-64" `X86_64 v4_12
      ~expected:["domains"; "domains+effects"];
    test "Multicore not on 4.10 i386" `I386 v4_10
      ~expect_exists:false ~expected:["domains"; "multicore"];
    test "Multicore not on 4.12 xi386" `I386 v4_12
      ~expect_exists:false ~expected:["domains"; "multicore"]
  ])

let () =
  Alcotest.run "ocaml-version" [ ("Compiler_variants", test_compiler_variants); ("Configure_options", test_configure_options) ]

let () =
  Alcotest.run "Mdx"
    [ Test_block.suite
    ; Test_library.suite
    ]

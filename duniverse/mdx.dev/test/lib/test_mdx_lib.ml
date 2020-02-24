let () =
  Alcotest.run "Mdx"
    [
      Test_block.suite;
      Test_label.suite;
      Test_dep.suite;
      Test_library.suite;
      Test_syntax.suite;
      Test_util.suite;
      Test_part.suite;
    ]

let () =
  Topfind.load_deeply ["core"; "async"];
  Toplevel_expect_test.Main.main ()

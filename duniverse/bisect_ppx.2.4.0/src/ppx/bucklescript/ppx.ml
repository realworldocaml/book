let () =
  let argc = Array.length Sys.argv in
  let argv =
    let output_file = Sys.argv.(argc - 1) in
    Array.(append
      (sub Sys.argv 0 (argc - 1))
      [|"-o"; output_file; "--dump-ast"; "--conditional"|])
  in

  Migrate_parsetree.Driver.run_main ~argv ()

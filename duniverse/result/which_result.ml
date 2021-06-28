let () =
  let version =
    Scanf.sscanf Sys.argv.(1) "%d.%d" (fun major minor -> (major, minor))
  in
  let file =
    if version < (4, 03) then
      "result-as-newtype.ml"
    else
      if version < (4, 08) then
        "result-as-alias.ml"
      else
        "result-as-alias-4.08.ml"
  in
  print_string file

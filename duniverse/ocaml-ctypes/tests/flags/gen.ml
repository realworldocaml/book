let () =
  let ocaml_version_str = Sys.ocaml_version in
  let ocaml_version =
    Scanf.sscanf ocaml_version_str "%u.%u" (fun a b -> (a, b))
  in
  if ocaml_version >= (4, 6) then
    print_endline ":standard"
  else
    print_endline "(:standard -ccopt -Wl,-E)"

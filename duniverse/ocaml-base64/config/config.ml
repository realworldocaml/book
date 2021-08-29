let parse s = Scanf.sscanf s "%d.%d" (fun major minor -> (major, minor))

let () =
  let version = parse Sys.ocaml_version in
  if version >= (4, 7)
  then print_string "unsafe_stable.ml"
  else print_string "unsafe_pre407.ml"

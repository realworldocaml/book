let write fn s =
  let oc = open_out fn in
  output_string oc s;
  close_out oc

let () =
  let ocaml_version_str = Sys.argv.(1) in
  let ocaml_version =
    Scanf.sscanf ocaml_version_str "%u.%u" (fun a b -> (a, b))
  in
  write "ast-version"
    (match ocaml_version with
    | 4, 02 -> "402"
    | 4, 03 -> "403"
    | 4, 04 -> "404"
    | 4, 05 -> "405"
    | 4, 06 -> "406"
    | 4, 07 -> "407"
    | 4, 08 -> "408"
    | 4, 09 -> "409"
    | 4, 10 -> "410"
    | 4, 11 -> "411"
    | 4, 12 -> "412"
    | 4, 13 -> "413"
    | 4, 14 -> "414"
    | _ ->
        Printf.eprintf "Unkown OCaml version %s\n" ocaml_version_str;
        exit 1)

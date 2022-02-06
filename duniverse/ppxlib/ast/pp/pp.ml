let () =
  match Sys.argv with
  | [| _; ocaml_version_str; fname |] ->
      let ocaml_version =
        match Supported_version.of_string ocaml_version_str with
        | Some v -> string_of_int (Supported_version.to_int v)
        | None ->
            Printf.eprintf "Unknown OCaml version %s\n" ocaml_version_str;
            exit 1
      in
      let ic = open_in_bin fname in
      Printf.printf "# 1 %S\n" fname;
      Pp_rewrite.rewrite ocaml_version (Lexing.from_channel ic)
  | _ ->
      Printf.eprintf "%s: <ocaml-version> <file-name>\n" Sys.executable_name;
      exit 2

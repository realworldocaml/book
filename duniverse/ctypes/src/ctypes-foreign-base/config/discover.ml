module C = Configurator.V1

let () =
  C.main ~name:"ffi" (fun c ->
      let default : C.Pkg_config.package_conf = {
        libs = ["-lffi"];
        cflags = []
      } in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
          (match C.Pkg_config.query pc ~package:"libffi" with
           | None -> default
           | Some v -> v)
      in
      let backend =
        match C.ocaml_config_var_exn c "system" with
        | "Win32" -> "win"
        | _ -> "unix" in

      let f = "as_needed_test" in
      let ml = f ^ ".ml" in
      open_out ml |> close_out;
      let res = C.Process.run_ok c "ocamlopt" ["-shared"; "-cclib"; "-Wl,--no-as-needed"; ml; "-o"; f^".cmxs"] in
      let extra_ldflags = if res then ["-Wl,--no-as-needed"] else [] in

      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_lines "c_flags" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" (conf.libs @ extra_ldflags);
      C.Flags.write_lines "backend.sexp" [backend]
    )

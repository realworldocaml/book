module C = Configurator.V1

let prepend opt flags =
  if flags = [] then
    []
  else
    opt :: flags

let () =
  C.main ~name:"fts_example" (fun c ->
      let default : C.Pkg_config.package_conf = {
        libs = [];
        cflags = []
      } in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
          (match C.Pkg_config.query pc ~package:"libfts" with
           | None -> default
           | Some v -> v)
      in
      C.Flags.write_sexp "c_flags.sexp" (prepend "-ccopt" conf.cflags);
      C.Flags.write_sexp "c_library_flags.sexp" (prepend "-cclib" conf.libs)
    )

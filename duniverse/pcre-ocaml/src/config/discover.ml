open Base

let () =
  let module C = Configurator.V1 in
  C.main ~name:"pcre" (fun c ->
    let default : C.Pkg_config.package_conf = {
      libs = ["-lpcre"];
      cflags = []
    } in
    let conf =
      Option.value_map (C.Pkg_config.get c) ~default ~f:(fun pc ->
        Option.value (C.Pkg_config.query pc ~package:"libpcre") ~default)
    in
    C.Flags.write_sexp "c_flags.sexp" conf.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" conf.libs)

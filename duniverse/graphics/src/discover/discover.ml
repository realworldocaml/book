module C = Configurator.V1

type os = Win32 | Darwin | Other

let os c =
  let win32 = C.ocaml_config_var c "os_type" = Some "Win32" in
  if win32 then Win32
  else
    match C.Process.run c "uname" [ "-s" ] with
    | { exit_code = 0; stdout; _ } when String.trim stdout = "Darwin" -> Darwin
    | _ -> Other

let from_pkg_config c =
  let fallback = ([], [ "-lX11" ]) in
  match C.Pkg_config.get c with
  | None -> fallback
  | Some pc -> (
      match C.Pkg_config.query pc ~package:"x11" with
      | None -> fallback
      | Some { cflags; libs } -> (cflags, libs))

let () =
  C.main ~name:"discover" (fun c ->
      let cflags, libs =
        match os c with
        | Other -> from_pkg_config c
        | Win32 -> ([], [ "-lkernel32"; "-lgdi32"; "-luser32" ])
        | Darwin ->
            let pkg_config_path =
              "/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig:/opt/X11/share/pkgconfig"
            in
            let pkg_config_path =
              match Sys.getenv "PKG_CONFIG_PATH" with
              | exception Not_found -> pkg_config_path
              | s -> s ^ ":" ^ pkg_config_path
            in
            Unix.putenv "PKG_CONFIG_PATH" pkg_config_path;
            from_pkg_config c
      in
      C.Flags.write_sexp "cflags" cflags;
      C.Flags.write_sexp "libs" libs)

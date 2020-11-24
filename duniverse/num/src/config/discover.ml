module C = Configurator.V1

let () =
  C.main ~name:"num" (fun c ->
    let arch =
      let arch =  C.ocaml_config_var_exn c "architecture"
      in
      match arch with
      | "amd64" -> "amd64"
      | "arm64" -> "arm64"
      | "power" -> "ppc"
      | "i386"  -> "ia32"
      | _ -> "bng_generic"
    in
    C.Flags.write_lines "arch" [arch];
    C.Flags.write_sexp "cflags" ["-DBNG_ARCH_"^arch]
  )

let () =
  let open Configurator.V1 in
  main ~name:"rng_flags" (fun _t ->
    let c_lib_flags =
      match Sys.os_type with
      | "Win32" | "Cygwin" -> ["-lbcrypt"]
      | _ -> []
    in
    Flags.write_sexp "rng_c_flags.sexp" c_lib_flags
  )

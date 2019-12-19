module C = Configurator.V1

let () =
  C.main ~name:"mirage-clock-unix" (fun c ->
    let ccflags =
      match C.ocaml_config_var c "system" with
      | Some "linux" -> ["-lrt"]
      | _ -> [] in
    C.Flags.write_sexp "cclib.sexp" ccflags;
  )

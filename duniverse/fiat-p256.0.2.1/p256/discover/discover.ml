module C = Configurator.V1

let c_program = "int main(void) {__int128 x;}"

let () =
  let output_file = "cflags.sexp" in
  C.main ~name:"fiat" (fun c ->
      let supported = C.c_test c c_program in
      let cflags = if supported then [ "-DOCAML_FIAT_USE_INT128" ] else [] in
      C.Flags.write_sexp output_file cflags)

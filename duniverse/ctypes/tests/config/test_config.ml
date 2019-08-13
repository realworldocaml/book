module C = Configurator.V1

let () =
  let ifile = ref "" in
  let cfile = ref "" in
  let args = [
    "-integers-dir", Arg.Set_string ifile, "location of ocaml_integers.h";
    "-ctypes-dir", Arg.Set_string cfile, "location of ctypes_cstubs_internals.h"] in
  C.main ~args ~name:"ctypes-tests" (fun _c ->
    let idir = ["-I";Filename.dirname !ifile] in
    let cdir = ["-I";Filename.dirname !cfile] in
    C.Flags.write_lines "test-cflags" (idir @ cdir)
  )

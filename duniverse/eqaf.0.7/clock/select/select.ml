let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let load_file filename =
  let ic = open_in_bin filename in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  let () = really_input ic rs 0 ln in
  Bytes.unsafe_to_string rs

let sexp_linux = "(-lrt)"

let sexp_windows = "()"

let sexp_mach = "()"

let () =
  let system, output =
    try
      match Sys.argv with
      | [|_; "--system"; system; "-o"; output|] ->
          let system =
            match system with
            | "linux" -> `Linux
            | "windows" | "mingw64" | "mingw" | "cygwin" -> `Windows
            | "macosx" -> `MacOSX
            | v -> invalid_arg "Invalid argument of system option: %s" v
          in
          (system, output)
      | _ -> invalid_arg "%s --system system -o <output>" Sys.argv.(0)
    with _ -> invalid_arg "%s --system system -o <output>" Sys.argv.(0)
  in
  let oc_ml, oc_c, oc_sexp =
    ( open_out (output ^ ".ml")
    , open_out (output ^ "_stubs.c")
    , open_out (output ^ ".sexp") )
  in
  let ml, c, sexp =
    match system with
    | `Linux ->
        ( load_file "clock_linux.ml"
        , load_file "clock_linux_stubs.c"
        , sexp_linux )
    | `Windows ->
        ( load_file "clock_windows.ml"
        , load_file "clock_windows_stubs.c"
        , sexp_windows )
    | `MacOSX ->
        (load_file "clock_mach.ml", load_file "clock_mach_stubs.c", sexp_mach)
  in
  Printf.fprintf oc_ml "%s%!" ml ;
  Printf.fprintf oc_c "%s%!" c ;
  Printf.fprintf oc_sexp "%s%!" sexp ;
  close_out oc_ml ;
  close_out oc_c ;
  close_out oc_sexp

let std_flags = ["--std=c11"; "-Wall"; "-Wextra"; "-Wpedantic"; "-O3"]

let () =
  let c = Configurator.V1.create "mirage-crypto" in
  let arch =
    let defines =
      Configurator.V1.C_define.import
        c
        ~includes:[]
        [("__x86_64__", Switch); ("__i386__", Switch)]
    in
    match defines with
    | (_, Switch true) :: _ -> `x86_64
    | _ :: (_, Switch true) :: _ -> `x86
    | _ -> `unknown
  in
  let accelerate_flags =
    match arch with
    | `x86_64 -> [ "-DACCELERATE"; "-mssse3"; "-maes"; "-mpclmul" ]
    | _ -> []
  in
  let ent_flags =
    match arch with
    | `x86_64 | `x86 -> [ "-DENTROPY"; "-mrdrnd"; "-mrdseed" ]
    | _ -> []
  in
  let flags = std_flags @ ent_flags in
  let opt_flags = flags @ accelerate_flags in
  Configurator.V1.Flags.write_sexp "cflags_optimized.sexp" opt_flags;
  Configurator.V1.Flags.write_sexp "cflags.sexp" flags

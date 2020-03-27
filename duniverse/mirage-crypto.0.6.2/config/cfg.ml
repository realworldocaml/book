let evar  = "MIRAGE_CRYPTO_ACCELERATE"
let needs = [`SSSE3; `AES; `PCLMULQDQ]
let flags = ["-DACCELERATE"; "-mssse3"; "-maes"; "-mpclmul"]
let std_flags = ["--std=c99"; "-Wall"; "-Wextra"; "-Wpedantic"; "-O3"]

let _ =
  let auto = match Cpuid.supports needs with Ok true -> flags | _ -> [] in
  let accelerate_flags = match Sys.getenv evar with
    | "true" -> flags
    | "false" -> []
    | _ -> auto
    | exception Not_found -> auto
  in
  let ent_flags =
    let c = Configurator.V1.create "mirage-crypto" in
    let arch = Configurator.V1.Process.run c "uname" ["-m"] in
    match String.trim arch.Configurator.V1.Process.stdout with
    | "x86_64" | "amd64" | "x86" -> [ "-mrdrnd" ; "-mrdseed" ]
    | _ -> []
  in
  let fs = std_flags @ ent_flags @ accelerate_flags in
  Format.(printf "(@[%a@])@.%!" (fun ppf -> List.iter (fprintf ppf "%s@ ")) fs)

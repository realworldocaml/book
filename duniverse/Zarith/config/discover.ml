module C = Configurator.V1
module OV = Ocaml_version
open Printf

(* Returns arch, ccdef, opt (cc/as)  *)
let x86_64_def =
  "x86_64", ["-DZ_ELF";"-DZ_DOT_LABEL_PREFIX"], []

let i686_def =
  "i686", ["-DZ_ELF";"-DZ_DOT_LABEL_PREFIX"], []

let cygwin_def word_size =
  match word_size with
  | "64" -> "x86_64_mingw64", ["-DZ_COFF"], []
  | _ -> "i686", ["-DZ_UNDERSCORE_PREFIX";"-DZ_COFF"], []

let darwin_def word_size =
  match word_size with
  | "64" -> "x86_64", ["-DZ_UNDERSCORE_PREFIX";"-DZ_MACOS"], []
  | _ -> failwith "Darwin only supports 64-bit compilation"

let arm_def = "arm", [], []

let no_def = "none", [], []

let extract_from_target word_size str =
  let reg_all = "\\(.*\\)"
  and reg_or = "\\|"
  in
  let x86_64 = Str.regexp ("x86_64"^reg_all^"-linux-gnu"^reg_or^"x86_64-kfreebsd-gnu")
  and i686 = Str.regexp ("i486-"^reg_all^"linux-gnu"^reg_or^"i686-"^reg_all^"linux-gnu"^reg_or^"i486-kfreebsd-gnu")
  and cygwin = Str.regexp ("i686-"^reg_all^"cygwin")
  and darwin = Str.regexp ("i386-"^reg_all^"darwin"^reg_all^reg_or^"x86_64-"^reg_all^"darwin"^reg_all)
  and arm = Str.regexp ("armv7"^reg_all^"-gnueabi")
  in
  if Str.string_match x86_64 str 0
  then x86_64_def
  else if Str.string_match i686 str 0
  then i686_def
  else if Str.string_match cygwin str 0
  then cygwin_def word_size
  else if Str.string_match darwin str 0
  then darwin_def word_size
  else if Str.string_match arm str 0
  then arm_def
  else
       no_def


let inc_gmp = {|
  #include <gmp.h>
  int main() {return 0;}
|}

let inc_mpir = {|
  #include <mpir.h>
  int main() {return 0;}
|}


let check_code c code lib = C.c_test c code ~link_flags:["-l"^lib]

let check_gmp c = match check_code c inc_gmp "gmp" with
  | true -> Some ("-DHAS_GMP", "-lgmp")
  | false -> None

let check_mpir c = match check_code c inc_mpir "mpir" with
  | true -> Some ("-DHAS_MPIR", "-lmpir")
  | false -> None

let check_gmp_or_mpir_raw c =
  if not(C.c_test c inc_gmp || C.c_test c inc_mpir)
  then failwith "Couldn't find GMP or MPIR"


let check_gmp_or_mpir c =
  match check_gmp c with
  | None -> check_mpir c
  | a -> a

let param_cflags = ref []
let param_ldflags = ref []

let set_cflags str = param_cflags := String.split_on_char ' ' str
let set_ldflags str = param_ldflags := String.split_on_char ' ' str

let arg_cflags = ("-cflags", Arg.String set_cflags, "custom C flags")
let arg_ldflags = ("-ldflags", Arg.String set_ldflags, "custom ld flags")

let args = [arg_cflags; arg_ldflags]

let () =
  C.main ~args ~name:"zarith" (fun c ->
    let word_size = C.ocaml_config_var_exn c "word_size" in
    let machine, arch_defines, opt = C.ocaml_config_var_exn c "target" |> extract_from_target word_size in
    let ov = C.ocaml_config_var_exn c "version" |> OV.of_string_exn in
    let stdlib_include = sprintf "-I%s" (C.ocaml_config_var_exn c "standard_library") in
    let cflags = stdlib_include :: ["-O3";"-Wall";"-Wextra"] in
    let defines = ["-DZ_OCAML_COMPARE_EXT"; "-DZ_OCAML_HASH"] in
    let defines, ldflags =match !param_cflags, !param_ldflags with
    | [], [] -> begin
                  match check_gmp_or_mpir c with
                  | Some (cflag,ldflag) -> cflag :: defines, [ldflag]
                  | None -> failwith "Cannot find GMP nor MPIR"
                end
    | c_flags, ld_flags ->  begin
                  match check_gmp_or_mpir c with
                  | Some (cflag,_) -> cflag :: defines @ c_flags, ld_flags
                  | None -> failwith "Cannot find GMP nor MPIR"
                end
    in
    let c_api_defines =
      match Ocaml_version.(compare ov Releases.v4_08) with
      |(-1) -> ["-DZ_OCAML_LEGACY_CUSTOM_OPERATIONS"]
      |_ -> [] in
    let defines = defines @ c_api_defines @ arch_defines in
    let cflags = cflags @ opt @ defines in
    let asflags = defines @ opt in
    C.Flags.write_sexp "cflags.sxp" cflags;
    C.Flags.write_lines "asflags" asflags;
    C.Flags.write_lines "cflags" cflags;
    C.Flags.write_sexp "ldflags.sxp" ldflags;
    C.Flags.write_lines "ldflags" ldflags;
    C.Flags.write_lines "arch" [machine])

(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module C = Configurator.V1

let header = "\
(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Support for various ABIs *)

[@@@warning \"-37\"]

type abi = Code of int | Unsupported of string

let abi_code = function
   Code c -> c
 | Unsupported sym -> raise (Ctypes.Unsupported sym)

"

let ffi_is_defined = Printf.sprintf "\
#include <ffi.h>
int main(int argc, char **argv) {
   int s = %s;
   return 0;
}
"

let symbols = [
  ("aix"              , "FFI_AIX");
  ("darwin"           , "FFI_DARWIN");
  ("eabi"             , "FFI_EABI");
  ("fastcall"         , "FFI_FASTCALL");
  ("gcc_sysv"         , "FFI_GCC_SYSV");
  ("linux"            , "FFI_LINUX");
  ("linux64"          , "FFI_LINUX64");
  ("linux_soft_float" , "FFI_LINUX_SOFT_FLOAT");
  ("ms_cdecl"         , "FFI_MS_CDECL");
  ("n32"              , "FFI_N32");
  ("n32_soft_float"   , "FFI_N32_SOFT_FLOAT");
  ("n64"              , "FFI_N64");
  ("n64_soft_float"   , "FFI_N64_SOFT_FLOAT");
  ("o32"              , "FFI_O32");
  ("o32_soft_float"   , "FFI_O32_SOFT_FLOAT");
  ("osf"              , "FFI_OSF");
  ("pa32"             , "FFI_PA32");
  ("stdcall"          , "FFI_STDCALL");
  ("sysv"             , "FFI_SYSV");
  ("thiscall"         , "FFI_THISCALL");
  ("unix"             , "FFI_UNIX");
  ("unix64"           , "FFI_UNIX64");
  ("v8"               , "FFI_V8");
  ("v8plus"           , "FFI_V8PLUS");
  ("v9"               , "FFI_V9");
  ("vfp"              , "FFI_VFP");
  ("default_abi"      , "FFI_DEFAULT_ABI");
]

let includes = ["ffi.h"]
module CD = C.C_define

let find_defined_symbols c c_flags =
  List.fold_left (fun acc (_,sym) ->
    if C.c_test c ~c_flags (ffi_is_defined sym) then
      sym :: acc
    else acc) [] symbols

let get_symbol c c_flags symbol =
  match CD.(import c ~includes ~c_flags [symbol,Type.Int]) with
  |[_,CD.Value.Int i] -> i
  |_ -> failwith (Printf.sprintf "unexpected error parsing ffi.h: is %s not an integer?" symbol)

let write_line c ~c_flags ~defined_symbols ~name ~symbol =
  if List.mem symbol defined_symbols then
    get_symbol c c_flags symbol |>
    Printf.printf "let %s = Code %d\n" name
  else
    Printf.printf "let %s = Unsupported \"%s\"\n" name symbol

let () =
  let c_flags = ref "" in
  let args = ["-cflags", Arg.Set_string c_flags, "CFLAGS for libffi"] in
  C.main ~args ~name:"ctypes-ffi" (fun c ->
    let c_flags = match !c_flags with "" -> [] | c -> [c] in
    let defined_symbols = find_defined_symbols c c_flags in
    print_string header;
    List.iter (fun (name, symbol) -> write_line c ~c_flags ~defined_symbols ~name ~symbol) symbols
  )


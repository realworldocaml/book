(*
 * Copyright (c)  Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let c_headers = "
#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>
#include <string.h>
"

let main () =
  let ml_out = open_out "fts_generated.ml"
  and c_out = open_out "fts_stubs.c" in
  let ml_fmt = Format.formatter_of_out_channel ml_out
  and c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s@\n" c_headers;
  Cstubs.write_c c_fmt ~prefix:"fts_stub_" (module Fts_bindings.Bindings);
  Cstubs.write_ml ml_fmt ~prefix:"fts_stub_" (module Fts_bindings.Bindings);
  Format.pp_print_flush ml_fmt ();
  Format.pp_print_flush c_fmt ();
  close_out ml_out;
  close_out c_out

let () = main ()

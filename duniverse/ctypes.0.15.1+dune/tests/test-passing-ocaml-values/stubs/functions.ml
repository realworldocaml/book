(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the OCaml-value-passing tests. *)

open Ctypes

let name_strdup =
  match Sys.os_type with
    | "Win32" -> "_strdup"
    | _ -> "strdup"

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  let memcpy_string_string = foreign "memcpy"
    (ocaml_string @-> ocaml_string @-> size_t @-> returning (ptr void))

  let memcpy_bytes_bytes = foreign "memcpy"
    (ocaml_bytes @-> ocaml_bytes @-> size_t @-> returning (ptr void))

  let memcpy_string_ptr = foreign "memcpy"
    (ocaml_string @-> ptr void @-> size_t @-> returning (ptr void))

  let strdup = foreign name_strdup
    (ocaml_string @-> returning string)
end

(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the variadic function tests. *)

open Ctypes

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  let size_t_as_int : int typ = view size_t
    ~read:Unsigned.Size_t.to_int
    ~write:Unsigned.Size_t.of_int

  let bind_snprintf tail =
    foreign "snprintf" (ptr char @-> size_t_as_int @-> string @-> tail)

  let snprintf_int =
    bind_snprintf (int @-> returning int)

  let snprintf_char_unsigned =
    bind_snprintf (char @-> uint @-> returning int)

  let snprintf_longlong_int =
    bind_snprintf (llong @-> int @-> returning int)

  let snprintf_string_ushort =
    bind_snprintf (string @-> ushort @-> returning int)
end

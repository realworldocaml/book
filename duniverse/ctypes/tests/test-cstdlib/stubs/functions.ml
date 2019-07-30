(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the C standard library tests. *)

open Ctypes
open Foreign

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  let cchar = view ~read:Char.chr ~write:Char.code int
  let bool = view ~read:((<>)0) ~write:(fun b -> if b then 1 else 0) int

  let t = (cchar @-> returning bool)

  let isalnum = foreign "isalnum" t
  and isalpha = foreign "isalpha" t
  and iscntrl = foreign "iscntrl" t
  and isdigit = foreign "isdigit" t
  and isgraph = foreign "isgraph" t
  and islower = foreign "islower" t
  and isprint = foreign "isprint" t
  and ispunct = foreign "ispunct" t
  and isspace = foreign "isspace" t
  and isupper = foreign "isupper" t
  and isxdigit = foreign "isxdigit" t

  (* char *strchr(const char *str, int c);  *)
  let strchr = foreign "strchr" (string @-> int @-> returning string)

  (* int strcmp(const char *str1, const char *str2);  *)
  let strcmp = foreign "strcmp" (string @-> string @-> returning int)

  (* int memcmp(const void *ptr1, const void *ptr2, size_t num) *)
  let memcmp = foreign "memcmp"
    (ptr void @-> ptr void @-> size_t @-> returning int)

  (* void  *memset(void *ptr, int value, size_t num) *)
  let memset = foreign "memset"
    (ptr void @-> int @-> size_t @-> returning (ptr void))

  (* let div = foreign "div" (int @-> int @-> returning div_t) *)

  let qsort = foreign "qsort"
    (ptr void @-> size_t @-> size_t @->
     funptr Ctypes.(ptr void @-> ptr void @-> returning int) @->
     returning void)

  let bsearch = foreign "bsearch"
    (ptr void @-> ptr void @-> size_t @-> size_t @->
     funptr Ctypes.(ptr void @-> ptr void @-> returning int) @->
     returning (ptr void))

  let strlen = foreign "strlen" (ptr char @-> returning size_t)
end

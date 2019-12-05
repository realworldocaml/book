(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes_static

(* The format context affects the formatting of pointer, struct and union
   types.  There are three printing contexts: *)
type format_context = [
(* In the top-level context struct and union types are printed in full, with
   member lists.  Pointer types are unparenthesized; for example,
   pointer-to-void is printed as "void *", not as "void ( * )". *)
| `toplevel
(* In the array context, struct and union types are printed in abbreviated
   form, which consists of just a keyword and the tag name.  Pointer types are
   parenthesized; for example, pointer-to-array-of-int is printed as
   "int ( * )[]", not as "int *[]". *)
| `array
(* In the non-array context, struct and union types are printed in abbreviated
   form and pointer types are unparenthesized. *)
| `nonarray]

val format_name : ?name:string -> Format.formatter -> unit

val format_typ' : 'a Ctypes_static.typ -> (format_context -> Format.formatter -> unit) ->
  format_context -> Format.formatter -> unit

val format_typ : ?name:string -> Format.formatter -> 'a typ -> unit

val format_fn' : 'a fn -> (Format.formatter -> unit) -> Format.formatter -> unit

val format_fn : ?name:string -> Format.formatter -> 'a fn -> unit

val string_of_typ : ?name:string -> 'a typ -> string

val string_of_fn : ?name:string -> 'a fn -> string

(** Code languages. *)
open Core.Std
open Async.Std

type t = [
| `Ascii
| `ATD
| `Bash
| `C
| `C_header
| `Console
| `CPP
| `Gas
| `Java
| `JSON
| `OCaml_ml
| `OCaml_lex
| `OCaml_mli
| `OCaml_pack
| `OCaml_rawtoplevel
| `OCaml_syntax
| `OCaml_toplevel
| `OCaml_yacc
| `Scheme
| `Shell
| `Shell_err
| `Shell_raw
| `Text
] with sexp

val to_docbook : t -> string Or_error.t

val to_extension : t -> string
val of_extension : string -> t Or_error.t

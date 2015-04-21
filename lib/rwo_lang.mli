(** Code languages. *)
open Core.Std
open Async.Std

type t = [
| `OCaml
| `OCaml_toplevel
| `OCaml_rawtoplevel
| `OCaml_syntax
| `Console
| `JSON
| `ATD
| `Scheme
| `C
| `Bash
| `CPP
| `Java
| `Ascii
| `Gas
] with sexp

val of_string : string -> t Or_error.t
val to_string : t -> string
val to_docbook : t -> string Or_error.t
val to_pygmentize : t -> string Or_error.t

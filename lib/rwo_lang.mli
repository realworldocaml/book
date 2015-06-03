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

(** Return file extensions for the given language. We return a list
    because sometimes multiple extensions are used for the same
    language, e.g. both ml and mli files are for OCaml.

    The empty list is also returned because some languages aren't
    actually supported, e.g. all .atd files are actually marked as
    OCaml files in import nodes. Probably we should thus delete [`ATD]
    from this module, but leaving it in case it is needed later. *)
val to_extensions : t -> string list

val of_extension : string -> t Or_error.t

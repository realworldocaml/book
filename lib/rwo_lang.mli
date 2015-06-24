(** Code languages. *)
open Core.Std
open Async.Std

type t = [
| `Ascii
| `ATD
| `Bash
| `C
| `Console
| `CPP
| `Gas
| `Java
| `JSON
| `OCaml
| `OCaml_rawtoplevel
| `OCaml_syntax
| `OCaml_toplevel
| `Scheme
] with sexp

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

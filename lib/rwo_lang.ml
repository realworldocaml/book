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

let to_docbook lang = match lang with
  | `ATD               -> Ok "ocaml"
  | `Bash              -> Ok "bash"
  | `C                 -> Ok "c"
  | `Console           -> Ok "console"
  | `CPP               -> Ok "c"
  | `Gas               -> Ok "gas"
  | `Java              -> Ok "java"
  | `JSON              -> Ok "json"
  | `OCaml             -> Ok "ocaml"
  | `Scheme            -> Ok "scheme"
  | `Ascii
  | `OCaml_rawtoplevel
  | `OCaml_syntax
  | `OCaml_toplevel ->
    error "language not supported by docbook" lang sexp_of_t

let to_extensions = function
  | `Bash -> ["cmd"]
  | `C -> ["c"; "cpp"; "h"]
  | `Console -> ["out"]
  | `Gas -> ["S"]
  | `Java -> ["java"]
  | `JSON -> ["json"]
  | `OCaml -> ["atd"; "ml"; "mli"; "mll"; "mly"]
  | `OCaml_rawtoplevel -> ["rawscript"]
  | `OCaml_syntax -> ["syntax"]
  | `OCaml_toplevel -> ["topscript"]
  | `Scheme -> ["scm"]
  | `Ascii | `ATD | `CPP -> []

let of_extension = function
  | "cmd" -> Ok `Bash
  | "c" | "cpp" | "h" -> Ok `C
  | "out" -> Ok `Console
  | "S" -> Ok `Gas
  | "java" -> Ok `Java
  | "json" -> Ok `JSON
  | "atd" | "ml" | "mli" | "mll" | "mly" -> Ok `OCaml
  | "rawscript" -> Ok `OCaml_rawtoplevel
  | "syntax" -> Ok `OCaml_syntax
  | "topscript" -> Ok `OCaml_toplevel
  | "scm" -> Ok `Scheme
  | x -> error "unknown extension" x sexp_of_string

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

let of_string = function
  | "ocaml"    -> Ok `OCaml
  | "ocamltop" -> Ok `OCaml_toplevel
  | "ocamlrawtop" -> Ok `OCaml_rawtoplevel
  | "console"  -> Ok `Console
  | "json"     -> Ok `JSON
  | "atd"      -> Ok `ATD
  | "scheme"   -> Ok `Scheme
  | "ocamlsyntax" -> Ok `OCaml_syntax
  | "java"     -> Ok `Java
  | "c"        -> Ok `C
  | "sh"       -> Ok `Bash
  | "cpp"      -> Ok `CPP
  | "ascii"    -> Ok `Ascii
  | "gas"      -> Ok `Gas
  | x          -> error "unknown code language" x sexp_of_string

let to_string = function
  | `OCaml             -> "ocaml"
  | `OCaml_toplevel    -> "ocamltop"
  | `OCaml_rawtoplevel -> "ocamlrawtop"
  | `Console           -> "console"
  | `JSON              -> "json"
  | `ATD               -> "atd"
  | `Scheme            -> "scheme"
  | `OCaml_syntax      -> "ocamlsyntax"
  | `Java              -> "java"
  | `C                 -> "c"
  | `Bash              -> "sh"
  | `CPP               -> "cpp"
  | `Ascii             -> "ascii"
  | `Gas               -> "gas"

let to_docbook lang = match lang with
  | `OCaml             -> Ok "ocaml"
  | `Console           -> Ok "console"
  | `JSON              -> Ok "json"
  | `ATD               -> Ok "ocaml"
  | `Scheme            -> Ok "scheme"
  | `Java              -> Ok "java"
  | `C                 -> Ok "c"
  | `Bash              -> Ok "bash"
  | `CPP               -> Ok "c"
  | `Gas               -> Ok "gas"
  | `OCaml_toplevel
  | `OCaml_rawtoplevel
  | `OCaml_syntax
  | `Ascii ->
    error "language not supported by docbook" lang sexp_of_t

let to_extensions = function
  | `Bash -> ["cmd"]
  | `C -> ["c"; "cpp"; "h"]
  | `Console -> ["out"]
  | `Scheme -> ["scm"]
  | `Gas -> ["S"]
  | `JSON -> ["json"]
  | `Java -> ["java"]
  | `OCaml -> ["atd"; "ml"; "mli"; "mll"; "mly"]
  | `OCaml_rawtoplevel -> ["rawscript"]
  | `OCaml_syntax -> ["syntax"]
  | `OCaml_toplevel -> ["topscript"]
  | `ATD | `CPP | `Ascii -> []

let of_extension = function
  | "cmd" -> Ok `Bash
  | "c" | "cpp" | "h" -> Ok `C
  | "out" -> Ok `Console
  | "scm" -> Ok `Scheme
  | "S" -> Ok `Gas
  | "json" -> Ok `JSON
  | "java" -> Ok `Java
  | "atd" | "ml" | "mli" | "mll" | "mly" -> Ok `OCaml
  | "rawscript" -> Ok `OCaml_rawtoplevel
  | "syntax" -> Ok `OCaml_syntax
  | "topscript" -> Ok `OCaml_toplevel
  | x -> error "unknown extension" x sexp_of_string

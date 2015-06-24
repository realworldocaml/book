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

let to_docbook lang = match lang with
  | `ATD               -> Ok "ocaml"
  | `Bash              -> Ok "bash"
  | `C                 -> Ok "c"
  | `C_header          -> Ok "c"
  | `Console           -> Ok "console"
  | `CPP               -> Ok "c"
  | `Gas               -> Ok "gas"
  | `Java              -> Ok "java"
  | `JSON              -> Ok "json"
  | `OCaml_ml
  | `OCaml_mli         -> Ok "ocaml"
  | `Scheme            -> Ok "scheme"
  | `Ascii
  | `OCaml_lex
  | `OCaml_pack
  | `OCaml_rawtoplevel
  | `OCaml_syntax
  | `OCaml_toplevel
  | `OCaml_yacc
  | `Shell
  | `Shell_err
  | `Shell_raw
  | `Text
    -> error "language not supported by docbook" lang sexp_of_t

let to_extension = function
  | `Ascii -> "ascii"
  | `ATD -> "atd"
  | `Bash -> "cmd"
  | `C -> "c"
  | `C_header -> "h"
  | `CPP -> "cpp"
  | `Console -> "out"
  | `Gas -> "S"
  | `Java -> "java"
  | `JSON -> "json"
  | `OCaml_ml -> "ml"
  | `OCaml_lex -> "mll"
  | `OCaml_mli -> "mli"
  | `OCaml_pack -> "mlpack"
  | `OCaml_rawtoplevel -> "rawscript"
  | `OCaml_syntax -> "syntax"
  | `OCaml_toplevel -> "topscript"
  | `OCaml_yacc -> "mly"
  | `Scheme -> "scm"
  | `Shell -> "sh"
  | `Shell_err -> "errsh"
  | `Shell_raw -> "rawsh"
  | `Text -> "txt"

let of_extension = function
  | "ascii" -> Ok `Ascii
  | "atd" -> Ok `ATD
  | "cmd" -> Ok `Bash
  | "c" -> Ok `C
  | "out" -> Ok `Console
  | "h" -> Ok `C_header
  | "cpp" -> Ok `CPP
  | "S" | "s" -> Ok `Gas
  | "java" -> Ok `Java
  | "json" -> Ok `JSON
  | "ml" -> Ok `OCaml_ml
  | "mll" -> Ok `OCaml_lex
  | "mli" -> Ok `OCaml_mli
  | "mlpack" -> Ok `OCaml_pack
  | "rawscript" -> Ok `OCaml_rawtoplevel
  | "syntax" -> Ok `OCaml_syntax
  | "topscript" -> Ok `OCaml_toplevel
  | "mly" -> Ok `OCaml_yacc
  | "scm" -> Ok `Scheme
  | "sh" -> Ok `Shell
  | "errsh" -> Ok `Shell_err
  | "rawsh" -> Ok `Shell_raw
  | "txt" -> Ok `Text
  | x -> error "unknown extension" x sexp_of_string

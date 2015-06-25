open Core.Std
open Async.Std

type t = string
with sexp

let to_string = Fn.id

let of_string x = match x with
  | "ascii"
  | "atd"
  | "c"
  | "cmd"
  | "cpp"
  | "errsh"
  | "h"
  | "java"
  | "json"
  | "ml"
  | "mli"
  | "mll"
  | "mlpack"
  | "mly"
  | "out"
  | "rawscript"
  | "rawsh"
  | "S" | "s"
  | "scm"
  | "sh"
  | "syntax"
  | "topscript"
  | "txt" -> Ok x
  | _ -> error "invalid extension" x sexp_of_string

let to_docbook_lang t = match t with
  | "atd"   -> Ok "ocaml"
  | "cmd"   -> Ok "bash"
  | "c"     -> Ok "c"
  | "h"     -> Ok "c"
  | "out"   -> Ok "console"
  | "cpp"   -> Ok "c"
  | "S"
  | "s"     -> Ok "gas"
  | "java"  -> Ok "java"
  | "json"  -> Ok "json"
  | "ml"
  | "mli"   -> Ok "ocaml"
  | "scm"   -> Ok "scheme"
  | _ -> error "language not supported by docbook" t sexp_of_t

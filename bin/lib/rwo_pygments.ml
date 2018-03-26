open Core
open Async
module Html = Rwo_html
module Lang = Rwo_lang

type lang = [
  | `OCaml
  | `Bash
  | `C
  | `Gas
  | `Java
  | `Json
  | `Scheme
  | `Sexp
]

let of_lang (x:Lang.t) = match (x :> string) with
  | "atd"          -> Ok `OCaml
  | "cmd"          -> Ok `Bash
  | "c"            -> Ok `C
  | "cpp"          -> Ok `C
  | "h"            -> Ok `C
  | "S"
  | "s"            -> Ok `Gas
  | "java"         -> Ok `Java
  | "json"         -> Ok `Json
  | "ml"
  | "mli"
  | "mlt"
  | "rawscript"    -> Ok `OCaml
  | "scm"          -> Ok `Scheme
  | "sexp"         -> Ok `Sexp
  | _ ->
    error "we are not supporting this language for pygmentize" x Lang.sexp_of_t

let lang_to_string = function
  | `OCaml -> "ocaml"
  | `Bash -> "bash"
  | `C -> "c"
  | `Gas -> "gas"
  | `Java -> "java"
  | `Json -> "json"
  | `Scheme -> "scheme"
  | `Sexp -> "scheme"

let pygmentize ?(add_attrs=[]) lang contents =
  contents
  |> (fun x -> Html.pre ~a:add_attrs [
      Html.code ~a:["class","language-" ^ (lang_to_string lang)]
        [`Data x]])
  |> return

open Core

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

let pygmentize ?(interactive=false) ?(add_attrs=[]) lang contents =
  let pre_attrs =
    if not interactive then add_attrs else
      match lang with
      | `Bash ->
        ("class", "command-line") ::
        ("data-user", "rwo") ::
        ("data-host", "lama") ::
        ("data-filter-output", ">") :: add_attrs
      | `OCaml ->
        ("class", "command-line") ::
        ("data-prompt", "#") ::
        ("data-filter-output", ">") :: add_attrs
      | _ -> add_attrs
  in
  contents
  |> (fun x -> Html.pre ~a:pre_attrs [
      Html.code ~a:["class","language-" ^ (lang_to_string lang)] [`Data x]])

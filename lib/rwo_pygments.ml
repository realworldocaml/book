open Core.Std
open Async.Std
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
]
with sexp

let of_lang x = match x with
  | `OCaml
  | `OCaml_toplevel
  | `OCaml_rawtoplevel
  | `ATD               -> Ok `OCaml
  | `JSON              -> Ok `Json
  | `Scheme            -> Ok `Scheme
  | `Java              -> Ok `Java
  | `C                 -> Ok `C
  | `Bash              -> Ok `Bash
  | `CPP               -> Ok `C
  | `Gas               -> Ok `Gas
  | `Console
  | `OCaml_syntax
  | `Ascii ->
    error "we are not supporting this language for pygmentize" x Lang.sexp_of_t

let lang_to_string = function
  | `OCaml -> "ocaml"
  | `Bash -> "bash"
  | `C -> "c"
  | `Gas -> "gas"
  | `Java -> "java"
  | `Json -> "json"
  | `Scheme -> "scheme"

let pygmentize lang contents =
  Process.create ~prog:"pygmentize"
    ~args:["-l"; lang_to_string lang; "-f"; "html"] ()
  >>= fun proc ->
  match proc with
  | Error e -> raise (Error.to_exn e)
  | Ok proc -> (
      Writer.write (Process.stdin proc) contents;
      Process.collect_output_and_wait proc
      >>| fun {Process.Output.stdout; stderr; exit_status} ->
      match exit_status with
      | Error (`Exit_non_zero x) ->
        failwithf "pygmentize exited with %d on:\n%s" x contents ()
      | Error (`Signal x) ->
        failwithf "pymentize exited with signal %s on:\n%s"
          (Signal.to_string x) contents ()
      | Ok () ->
        if stderr <> "" then
          failwithf "pymentize exited with errors:\n%s\n%s" contents stderr ()
        else
          Html.of_string stdout |> List.hd_exn
    )

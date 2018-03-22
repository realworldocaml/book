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

let really_pygmentize ~add_attrs lang contents =
  let arg = match lang with
  | `OCaml ->
    ["-x"]
  | _ ->
    []
  in
  Process.create ~prog:"pygmentize"
    ~args:(arg @  ["-v";"-l"; lang_to_string lang; "-f"; "html"]) ()
  >>= fun proc ->
  match proc with
  | Error e -> raise (Error.to_exn e)
  | Ok proc -> (
      Writer.write (Process.stdin proc) contents;
      Process.collect_output_and_wait proc
      >>| fun {Process.Output.stdout; stderr; exit_status} ->
      match exit_status with
      | Error (`Exit_non_zero x) ->
        failwithf "pygmentize exited with %d on:\n%s\nOutput:%s\nError:%s " x contents stdout stderr ()
      | Error (`Signal x) ->
        failwithf "pygmentize exited with signal %s on:\n%s"
          (Signal.to_string x) contents ()
      | Ok () ->
        if stderr <> "" then
          failwithf "pygmentize exited with errors:\n%s\n%s" contents stderr ()
        else
          String.substr_replace_all ~pattern:"<span class=\"err\">\027</span>" ~with_:"" stdout |>
          String.substr_replace_all ~pattern:"<span class=\"o\">[0m</span>" ~with_:"</span>" |>
          String.substr_replace_all ~pattern:"<span class=\"o\">[38;5;236m</span>" ~with_:"<span style=\"color:#303030\">" |>
          String.substr_replace_all ~pattern:"<span class=\"o\">[38;5;4m</span>" ~with_:"<span style=\"color:#000080\">" |>
          String.substr_replace_all ~pattern:"<span class=\"o\">[38;5;9m</span>" ~with_:"<span style=\"color:#ff0000\">" |>
          Html.of_string |> List.hd_exn |> function
          | `Element {
              Html.name="div";
              attrs=["class","highlight"];
              childs=[`Element {Html.name="pre" as name; attrs; childs}];
            } ->
            `Element {Html.name; attrs=attrs@add_attrs; childs}
          | _ ->
            failwith "pygmentize output HTML not in form"
    )

let pygmentize ?(add_attrs=[]) ?(pygmentize=true) lang contents =
  if pygmentize then (
    really_pygmentize ~add_attrs lang contents )
  else
    (contents
    |> (fun x -> Html.pre ~a:add_attrs [Html.code ~a:["class","language-" ^ (lang_to_string lang)] [`Data x]])
    |> return)

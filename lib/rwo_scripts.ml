open Core
open Async
module Bash_script = Rwo_bash_script
module Html = Rwo_html
module Import = Rwo_import
module Lang = Rwo_lang
module Pygments = Rwo_pygments
module Expect = Rwo_expect

type part = string
  [@@deriving sexp]

type script = [
  | `OCaml of Expect.Raw_script.t
  | `OCaml_toplevel of Expect.Document.t
  | `OCaml_rawtoplevel of Expect.Raw_script.t
  | `Other of string
]

type script_part = [
  | `OCaml of Expect.Raw_script.part
  | `OCaml_toplevel of Expect.Chunk.t list
  | `OCaml_rawtoplevel of Expect.Raw_script.part
  | `Other of string
]

type t = script String.Map.t

(******************************************************************************)
(* Map-style Operations                                                       *)
(******************************************************************************)

let empty = String.Map.empty

let is_rawpart ~name p = name = p.Expect.Raw_script.name

let is_part ~name p = name = p.Expect.Part.name

(*let find (t:t) ?part:(name="") ~filename =
  match String.Map.find t filename with
  | None -> None
  | Some (`OCaml parts) -> (
      match List.find ~f:(is_rawpart ~name) parts with
      | None -> None
      | Some x -> Some (`OCaml x)
    )
  | Some (`OCaml_toplevel doc) -> (
      match List.find ~f:(is_part ~name) (Expect.Document.parts doc) with
      | None -> None
      | Some x -> Some (`OCaml_toplevel x.Expect.Part.chunks)
    )
  | Some (`OCaml_rawtoplevel parts) -> (
      match List.find ~f:(is_rawpart ~name) parts with
      | None -> None
      | Some x -> Some (`OCaml_rawtoplevel x)
    )
  | Some (`Other _ as x) ->
    if name = "" then Some x else None
*)

let find_exn t ?part:(name="") ~filename =
  let no_file_err() =
    ok_exn (error "no data for file" filename sexp_of_string)
  in
  let no_part_err() = ok_exn (
      error "no data for requested part of file"
        (filename,name) [%sexp_of: string * part] )
  in
  match String.Map.find t filename with
  | None -> no_file_err()
  | Some (`OCaml parts) -> (
      match List.find ~f:(is_rawpart ~name) parts with
      | None -> no_part_err()
      | Some x -> `OCaml x
    )
  | Some (`OCaml_toplevel doc) -> (
      match List.find ~f:(is_part ~name) (Expect.Document.parts doc) with
      | None -> no_part_err()
      | Some x -> `OCaml_toplevel x.Expect.Part.chunks
    )
  | Some (`OCaml_rawtoplevel parts) -> (
      match List.find ~f:(is_rawpart ~name) parts with
      | None -> no_part_err()
      | Some x -> `OCaml_rawtoplevel x
    )
  | Some (`Other _ as x) ->
    if name = "" then x else no_part_err()

let file_is_mem = Map.mem


(******************************************************************************)
(* Printers                                                                   *)
(******************************************************************************)
let phrases_to_html ?(pygmentize=false) phrases =

  let in_phrase (x : Expect.Chunk.t) : Html.item Deferred.t =
    match String.split (Expect.Chunk.code x) ~on:'\n' with
    | [] -> assert false
    | x::xs ->
      let x = sprintf "# %s" x in
      let phrase = String.concat ~sep:"\n  " (x::xs) in
      Pygments.pygmentize ~pygmentize `OCaml phrase
  in

  let string_of_responses responses =
    String.concat ~sep:"\n" (List.map ~f:snd responses)
  in

  (* get warnings or errors *)
  let messages x : Html.item option =
     (
       if Expect.Chunk.evaluated x then
         Expect.Chunk.warnings x
       else
         string_of_responses (Expect.Chunk.responses x)
     )
     |> function
     | "" -> None
     | x -> Some Html.(pre [`Data (x |> Html.encode)])
  in

  let stdout x : Html.item option =
    if Expect.Chunk.evaluated x then
      match Expect.Chunk.stdout x with
      | "" -> None
      | x -> Some Html.(pre [`Data (Html.encode x)])
    else None
  in

  let out_phrase (x : Expect.Chunk.t)
    : Html.item list Deferred.t
    =
    if Expect.Chunk.evaluated x then (
      let highlight = function
        | Expect.Chunk.OCaml, str ->
          Pygments.pygmentize ~add_attrs:["class","ge"] ~pygmentize `OCaml str
        | Expect.Chunk.Raw, str ->
          Pygments.pygmentize ~add_attrs:["class","ge"] ~pygmentize:false `OCaml str
      in
      Deferred.all (List.map ~f:highlight (Expect.Chunk.responses x))
    ) else
      Deferred.return []
  in

  let phrase_to_html (x : Expect.Chunk.t) : Html.t Deferred.t =
    (in_phrase x >>| Option.some) >>= fun in_phrase ->
    out_phrase x >>| fun out_phrase ->
    List.filter_map ~f:Fn.id [in_phrase; messages x; stdout x] @ out_phrase
  in

  Deferred.List.map phrases ~f:phrase_to_html
  >>| List.concat


let script_part_to_html ?(pygmentize=false) (x : script_part) =
  (
    match x with
    | `OCaml_toplevel phrases -> phrases_to_html ~pygmentize phrases
    | `OCaml x
    | `OCaml_rawtoplevel x ->
      let content = x.Expect.Raw_script.content in
      (Pygments.pygmentize ~pygmentize `OCaml content >>| fun x -> [x])
    | `Other x ->
      (Pygments.pygmentize ~pygmentize:false `OCaml x >>| fun x -> [x])
  ) >>| fun l ->
  Html.div ~a:["class","highlight"] l


(******************************************************************************)
(* Main Operations                                                            *)
(******************************************************************************)
let eval_script lang ~filename =
  match (lang : Lang.t :> string) with
  | "ml" | "mli" | "mly" | "mll" -> (
      (* Hack: Oloop.Script.of_file intended only for ml files but
         happens to work for mli, mll, and mly files. *)
      Expect.Raw_script.of_file ~filename
      >>|? fun script -> `OCaml script
    )
  | "rawtopscript" -> (
      Expect.Raw_script.of_file ~filename
      >>|? fun script -> `OCaml_rawtoplevel script
    )
  | "topscript" -> (
      (*if String.is_suffix filename ~suffix:"async/main.topscript" then (
        Expect.Raw_script.of_file ~filename
        >>|? fun script -> `OCaml_rawtoplevel script
      ) else*)
      (
        Expect.Document.of_file ~filename
        >>=? fun doc ->
        let corrected_built_filename =
          Filename.realpath filename ^ ".corrected"
        in
        let corrected_filename =
          corrected_built_filename
          |> Filename.parts
          |> List.filter ~f:(fun x -> x <> "_build")
          |> Filename.of_parts
        in
        if not doc.Expect.Document.matched then
          Log.Global.error "%s didn't match expect test" filename;
        Deferred.ok begin
          Sys.file_exists corrected_built_filename >>= function
          | `Yes ->
            Sys.rename corrected_built_filename corrected_filename;
          | `Unknown -> return ()
          | `No ->
            Sys.file_exists corrected_filename >>= function
            | `Yes -> Sys.remove corrected_filename
            | _ -> return ()
        end
        >>|? fun () -> `OCaml_toplevel doc
      )
    )
  | "sh" -> (
      Bash_script.eval_file filename >>|? fun x ->
      if not (List.for_all x.Bash_script.Evaluated.commands
                ~f:(fun x -> x.Bash_script.Evaluated.exit_code = 0))
      then
        Log.Global.error
          "all commands in %s expected to exit with 0 but got non-zero"
          filename
      ;
      `Other (Bash_script.Evaluated.to_string x)
    )
  | "errsh" -> (
      Bash_script.eval_file filename >>|? fun x ->
      if not (List.exists x.Bash_script.Evaluated.commands
                ~f:(fun x -> x.Bash_script.Evaluated.exit_code <> 0))
      then
        Log.Global.error
          "all commands in %s exited with 0 but expected at least one non-zero"
          filename
      ;
      `Other (Bash_script.Evaluated.to_string x)
    )
  | _ -> (
      Reader.file_contents filename >>| fun x ->
      Ok (`Other x)
    )

let add_script t lang ~filename =
  let dir,file = filename in
  let filename = Filename.concat dir file in
  if file_is_mem t file then
    return (error "script already exists" file sexp_of_string)
  else
    eval_script lang ~filename >>|? fun script ->
    Map.add t ~key:file ~data:script

let of_html ~filename html =
  let dir = Filename.dirname filename in
  let imports =
    Import.find_all html
    |> List.dedup ~compare:(fun i j -> compare i.Import.href j.Import.href)
  in
  Deferred.Or_error.List.fold imports ~init:empty ~f:(fun accum i ->
      add_script accum (Import.lang_of i |> ok_exn) ~filename:(dir,i.Import.href)
    )

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
] [@@deriving sexp]

type script_part = [
  | `OCaml of Expect.Raw_script.part
  | `OCaml_toplevel of Expect.Chunk.t list
  | `OCaml_rawtoplevel of Expect.Raw_script.part
  | `Other of string
] [@@deriving sexp]

type t = script String.Map.t

(******************************************************************************)
(* Map-style Operations                                                       *)
(******************************************************************************)

let empty = String.Map.empty

let is_rawpart ~name p = name = p.Expect.Raw_script.name

let is_part ~name p = name = p.Expect.Part.name

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
          Pygments.pygmentize ~add_attrs:["class","ge"]
            ~pygmentize `OCaml str
        | Expect.Chunk.Raw, str ->
          Pygments.pygmentize ~add_attrs:["class","ge"]
            ~pygmentize:false `OCaml str
      in
      Deferred.List.map ~f:highlight (Expect.Chunk.responses x)
    ) else
      Deferred.return []
  in

  let phrase_to_html (x : Expect.Chunk.t) : Html.t Deferred.t =
    let%bind in_phrase = (in_phrase x >>| Option.some) in
    let%map out_phrase = out_phrase x in
    List.filter_map ~f:Fn.id [in_phrase; messages x; stdout x] @ out_phrase
  in

  Deferred.List.map phrases ~f:phrase_to_html
  >>| List.concat


let script_part_to_html ?(pygmentize=false) (x : script_part) =
  let singleton x = [x] in
  let%map l =
    match x with
    | `OCaml_toplevel phrases -> phrases_to_html ~pygmentize phrases
    | `OCaml x
    | `OCaml_rawtoplevel x ->
      let content = x.Expect.Raw_script.content in
      Pygments.pygmentize ~pygmentize `OCaml content >>| singleton
    | `Other x ->
      Pygments.pygmentize ~pygmentize:false `OCaml x >>| singleton
  in
  Html.div ~a:["class","highlight"] l


let exn_of_filename filename content =
  let (_filename, extension) = Filename.split_extension filename in
  match extension with
  | Some ext -> (match ext with
    | "ml" | "mli" | "mly" | "mll" ->
      `OCaml Rwo_expect.Raw_script.{ name = ""; content }
    | "rawtopscript" ->
      `OCaml_rawtoplevel Rwo_expect.Raw_script.{ name = ""; content }
    | _ -> `Other content)
  | None -> `Other content

(******************************************************************************)
(* Main Operations                                                            *)
(******************************************************************************)
let eval_script lang ~filename =
  let open Deferred.Or_error.Let_syntax in
  match (lang : Lang.t :> string) with
  | "topscript" -> assert false (* handled directly by ocaml-topexepct *)
  | "ml" | "mli" | "mly" | "mll" -> (
      (* Hack: Oloop.Script.of_file intended only for ml files but
         happens to work for mli, mll, and mly files. *)
      let%map script = Expect.Raw_script.of_file ~filename in
      `OCaml script
    )
  | "rawtopscript" -> (
      let%map script = Expect.Raw_script.of_file ~filename in
      `OCaml_rawtoplevel script
    )
  | "sh" -> (
      let%map x = Bash_script.eval_file filename in
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
      let%map x = Bash_script.eval_file filename in
      if not (List.exists x.Bash_script.Evaluated.commands
                ~f:(fun x -> x.Bash_script.Evaluated.exit_code <> 0))
      then
        Log.Global.error
          "all commands in %s exited with 0 but expected at least one non-zero"
          filename
      ;
      `Other (Bash_script.Evaluated.to_string x)
    )
  | "sexp" when Filename.basename filename = "jbuild" ->
    let open Deferred.Let_syntax in
    let%map x = Reader.file_contents filename in
    let regexp = Str.regexp "ppx_sexp_conv -no-check" in
    let removed_check = Str.replace_first regexp "ppx_sexp_conv" x in
    let regexp = Str.regexp_string "(jbuild_version 1)" in
    let removed_check = Str.replace_first regexp "" removed_check in
    let regexp = Str.regexp_string "(include jbuild.inc)" in
    let removed_check = Str.replace_first regexp "" removed_check in
    begin match Sexplib.Sexp.of_string (sprintf "(%s)" removed_check) with
    | Atom _ -> assert false
    | List l ->
      let r =
        String.concat ~sep:"\n" (List.map ~f:Sexp_pretty.sexp_to_string l)
      in
      Ok (`Other r)
    end
  | _ ->
    let open Deferred.Let_syntax in
    let%map x = Reader.file_contents filename in
    Ok (`Other x)


let eval_script_to_sexp lang ~filename =
  let open Deferred.Or_error.Monad_infix in
  eval_script lang ~filename >>|
  sexp_of_script

let add_script t ~filename =
  let dir,file = filename in
  let filename = Filename.concat dir file in
  if file_is_mem t file then
    return (error "script already exists" file sexp_of_string)
  else begin
    let cache_filename = filename ^ ".sexp" in
    let%map script =
      match%bind Sys.file_exists cache_filename with
      | `Yes -> Async_unix.Reader.load_sexp cache_filename script_of_sexp
      | _    -> return (error "missing dependency" cache_filename sexp_of_string)
    in
    Result.map script ~f:(fun script ->
      Map.set t ~key:file ~data:script)
  end

let of_html ?(code_dir="examples") ~filename:_  html =
  let imports =
    Import.find_all html
    |> List.dedup_and_sort ~compare:(fun i j ->
      compare i.Import.href j.Import.href)
  in
  Deferred.Or_error.List.fold imports ~init:empty ~f:(fun accum i ->
      add_script accum ~filename:(code_dir,i.Import.href)
    )

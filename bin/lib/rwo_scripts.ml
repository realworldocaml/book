open Core
open Async
module Html = Rwo_html
module Import = Rwo_import
module Lang = Rwo_lang
module Pygments = Rwo_pygments
module Expect = Rwo_expect

type part = string
  [@@deriving sexp]

type script = [
  | `OCaml of Expect.Raw_script.t
  | `OCaml_toplevel of Expect.Mlt.t
  | `OCaml_rawtoplevel of Expect.Raw_script.t
  | `Shell of Expect.Cram.t
  | `Other of string
] [@@deriving sexp]

type script_part = [
  | `OCaml of Expect.Raw_script.part
  | `OCaml_toplevel of Expect.Chunk.t list
  | `OCaml_rawtoplevel of Expect.Raw_script.part
  | `Shell of Expect.Cram.t
  | `Other of string
] [@@deriving sexp]

type t = script String.Map.t

(******************************************************************************)
(* Map-style Operations                                                       *)
(******************************************************************************)

let empty = String.Map.empty

let is_rawpart ~name p = name = p.Expect.Raw_script.name

let is_part ~name p = name = Expect.Part.name p

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
      match List.find ~f:(is_part ~name) (Expect.Mlt.parts doc) with
      | None -> no_part_err()
      | Some x -> `OCaml_toplevel (Expect.Part.chunks x)
    )
  | Some (`OCaml_rawtoplevel parts) -> (
      match List.find ~f:(is_rawpart ~name) parts with
      | None -> no_part_err()
      | Some x -> `OCaml_rawtoplevel x
    )
  | Some (`Shell parts) -> (
      if name = "" then `Shell parts
      else match Expect.Cram.part name parts with
        | None   -> no_part_err ()
        | Some x -> `Shell x
    )
  | Some (`Other _ as x) ->
    if name = "" then x else no_part_err()

let file_is_mem = Map.mem


(******************************************************************************)
(* Printers                                                                   *)
(******************************************************************************)

let escape s =
  (* workaround a bug in prism.js command-line plugin... *)
  Re.replace Re.(compile @@ str "<") ~f:(fun _ -> "&#60;") s

let phrases_to_html phrases =

  let in_phrase x = Expect.Chunk.code x in

  let string_of_responses responses =
    String.concat ~sep:"\n" (List.map ~f:snd responses)
  in

  let prefix s =
    let s = String.strip (escape s) in
    let s = String.split_lines s in
    ">" ^ String.concat ~sep:"\n>" s
  in

  (* get warnings or errors *)
  let messages x =
    ( if Expect.Chunk.evaluated x then Expect.Chunk.warnings x
      else string_of_responses (Expect.Chunk.responses x))
    |> function
    | "" -> None
    | x  -> Some (prefix x)
  in

  let stdout x =
    if Expect.Chunk.evaluated x then
      match Expect.Chunk.stdout x with
      | "" -> None
      | x  -> Some (prefix x)
    else None
  in

  let out_phrase x =
    if Expect.Chunk.evaluated x then (
      let highlight = function
        | Expect.Chunk.OCaml, str
        | Expect.Chunk.Raw  , str -> prefix str
      in
      List.map ~f:highlight (Expect.Chunk.responses x)
    ) else
      []
  in

  let phrase_to_html x =
    let in_phrase = in_phrase x |> Option.some in
    let out_phrase = out_phrase x in
    List.filter_map ~f:Fn.id [in_phrase; messages x; stdout x] @ out_phrase
    |> String.concat ~sep:"\n"
  in

  let phrases = List.map phrases ~f:phrase_to_html in
  Pygments.pygmentize ~interactive:true `OCaml (String.concat ~sep:"\n" phrases)


let script_part_to_html (x : script_part) =
  let l =
    match x with
    | `OCaml_toplevel phrases -> phrases_to_html phrases
    | `OCaml_rawtoplevel x
    | `OCaml x -> Pygments.pygmentize `OCaml x.Expect.Raw_script.content
    | `Shell x -> Pygments.pygmentize ~interactive:true `Bash (Expect.Cram.to_html x)
    | `Other x -> Pygments.pygmentize `OCaml x
  in
  Html.div ~a:["class","highlight"] [l]


let exn_of_filename filename content =
  let (_filename, extension) = Filename.split_extension filename in
  match extension with
  | Some ext -> (match ext with
    | "ml" | "mli" | "mly" | "mll" ->
      `OCaml Rwo_expect.Raw_script.{ name = ""; content }
    | "rawscript" ->
      `OCaml_rawtoplevel Rwo_expect.Raw_script.{ name = ""; content }
    | _ -> `Other content)
  | None -> `Other content

(******************************************************************************)
(* Main Operations                                                            *)
(******************************************************************************)
let script lang ~filename =
  let open Deferred.Or_error.Let_syntax in
  match (lang : Lang.t :> string) with
  | "mlt" ->
    let%map script = Expect.Mlt.of_file ~filename in
    `OCaml_toplevel script
  | "ml" | "mli" | "mly" | "mll" -> (
      (* Hack: Oloop.Script.of_file intended only for ml files but
         happens to work for mli, mll, and mly files. *)
      let%map script = Expect.Raw_script.of_file ~filename in
      `OCaml script
    )
  | "rawscript" -> (
      let%map script = Expect.Raw_script.of_file ~filename in
      `OCaml_rawtoplevel script
    )
  | "sh" | "errsh" -> (
      let %map script = Expect.Cram.of_file ~filename in
      `Shell script
    )
  | "jbuild" ->
    let open Deferred.Let_syntax in
    let%map x = Reader.file_contents filename in
    let changes = [
        "ppx_sexp_conv -no-check", "ppx_sexp_conv";
        "(jbuild_version 1)"     , "";
        "(include jbuild.inc)"   , "";
      ] in
    let y =
      let re = Re.alt (List.map ~f:(fun (s, _) -> Re.str s) changes) in
      let f m = Caml.List.assoc (Re.Group.get m 0) changes in
      Re.replace (Re.compile re) ~f x
    in
    Ok (`Other y)
  | _ ->
    let open Deferred.Let_syntax in
    let%map x = Reader.file_contents filename in
    Ok (`Other x)

let add_script t lang ~filename =
  let dir,file = filename in
  let filename = Filename.concat dir file in
  if file_is_mem t file then
    return (error "script already exists" file sexp_of_string)
  else begin
    let%map script = script lang ~filename in
    Result.map script ~f:(fun script -> Map.set t ~key:file ~data:script)
  end

let of_html ?(code_dir="examples") ~filename:_  html =
  let imports =
    Import.find_all html
    |> List.dedup_and_sort ~compare:(fun i j ->
      compare i.Import.href j.Import.href)
  in
  Deferred.Or_error.List.fold imports ~init:empty ~f:(fun accum i ->
      let lang = Import.lang_of i |> ok_exn in
      add_script accum lang ~filename:(code_dir,i.Import.href)
    )

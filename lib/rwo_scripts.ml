open Core.Std
open Async.Std
module Html = Rwo_html
module Import = Rwo_import
module Pygments = Rwo_pygments
let (/) = Filename.concat

type script = [
| `OCaml of Oloop.Script.t
| `OCaml_toplevel of Oloop.Script.Evaluated.t
| `OCaml_rawtoplevel of Oloop.Script.t
| `Other of string
]

type script_part = [
| `OCaml of string
| `OCaml_toplevel of Oloop.Script.Evaluated.phrase list
| `OCaml_rawtoplevel of string
| `Other of string
]

type t = script String.Map.t


(******************************************************************************)
(* Map-style Operations                                                       *)
(******************************************************************************)
let empty = String.Map.empty

let of_script (parts:Oloop.Script.t) : (float * string) list =
  List.map
    (parts : Oloop.Script.t :> Oloop.Script.part list)
    ~f:(fun {Oloop.Script.number; content} -> number,content)

let find (t:t) ?(part=0.) ~filename =
  match String.Map.find t filename with
  | None -> None
  | Some (`OCaml parts) -> (
    match List.Assoc.find ~equal:Float.equal (of_script parts) part with
    | None -> None
    | Some x -> Some (`OCaml x)
  )
  | Some (`OCaml_toplevel script) -> (
    match Oloop.Script.Evaluated.nth script part with
    | None -> None
    | Some x -> Some (`OCaml_toplevel x.Oloop.Script.Evaluated.phrases)
  )
  | Some (`OCaml_rawtoplevel parts) -> (
    match List.Assoc.find ~equal:Float.equal (of_script parts) part with
    | None -> None
    | Some x -> Some (`OCaml_rawtoplevel x)
  )
  | Some (`Other _ as x) ->
    if part = 0. then Some x else None

let find_exn t ?(part=0.) ~filename =
  let no_file_err() =
    ok_exn (error "no data for file" filename sexp_of_string)
  in
  let no_part_err() = ok_exn (
    error "no data for requested part of file"
      (filename,part) <:sexp_of< string * float >> )
  in
  match String.Map.find t filename with
  | None -> no_file_err()
  | Some (`OCaml parts) -> (
    match List.Assoc.find ~equal:Float.equal (of_script parts) part with
    | None -> no_part_err()
    | Some x -> `OCaml x
  )
  | Some (`OCaml_toplevel script) -> (
    match Oloop.Script.Evaluated.nth script part with
    | None -> no_part_err()
    | Some x -> `OCaml_toplevel x.Oloop.Script.Evaluated.phrases
  )
  | Some (`OCaml_rawtoplevel parts) -> (
    match List.Assoc.find ~equal:Float.equal (of_script parts) part with
    | None -> no_part_err()
    | Some x -> `OCaml_rawtoplevel x
  )
  | Some (`Other _ as x) ->
    if part = 0. then x else no_part_err()

let file_is_mem = Map.mem


(******************************************************************************)
(* Printers                                                                   *)
(******************************************************************************)
let phrases_to_html ?(pygmentize=false) phrases =

  let in_phrase (x:Oloop.Script.Evaluated.phrase) : Html.item Deferred.t =
    match String.split x.Oloop.Script.Evaluated.phrase ~on:'\n' with
    | [] -> assert false
    | x::xs ->
      let x = sprintf "# %s" x in
      let phrase = String.concat ~sep:"\n  " (x::xs) in
      Pygments.pygmentize ~pygmentize `OCaml phrase
  in

  (* get warnings or errors *)
  let messages (x:Oloop.Script.Evaluated.phrase) : Html.item option =
    (
      match x.Oloop.Script.Evaluated.outcome with
      | `Uneval (_,msg) -> [msg]
      | `Eval e ->
        Oloop.Outcome.warnings e
        |> List.map ~f:(fun (loc,warning) ->
            let buf = Buffer.create 256 in
            let fmt = Format.formatter_of_buffer buf in
            Location.print_loc fmt loc;
 	    ignore (Warnings.print fmt warning);
            Buffer.contents buf
          )
    )
    |> function
    | [] -> None
    | l -> Some Html.(pre [`Data (String.concat l ~sep:"\n")])
  in

  let stdout (x:Oloop.Script.Evaluated.phrase) : Html.item option =
    match x.Oloop.Script.Evaluated.outcome with
    | `Uneval _ -> None
    | `Eval e -> match Oloop.Outcome.stdout e with
      | "" -> None
      | x -> Some Html.(pre [`Data x])
  in

  let out_phrase (x:Oloop.Script.Evaluated.phrase)
    : Html.item option Deferred.t
    =
    match x.Oloop.Script.Evaluated.outcome with
    | `Uneval _ -> return None
    | `Eval e ->
      let buf = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buf in
      !Oprint.out_phrase fmt (Oloop.Outcome.result e);
      Buffer.contents buf
      |> Pygments.pygmentize ~pygmentize `OCaml
      >>| Option.some
  in

  let phrase_to_html (x:Oloop.Script.Evaluated.phrase) : Html.t Deferred.t =
    (in_phrase x >>| Option.some) >>= fun in_phrase ->
    out_phrase x >>| fun out_phrase ->
    [in_phrase; messages x; stdout x; out_phrase]
    |> List.filter_map ~f:Fn.id
  in

  Deferred.List.map phrases ~f:phrase_to_html
  >>| List.concat


let script_part_to_html ?(pygmentize=false) x =
  (
  match x with
  | `OCaml_toplevel phrases -> phrases_to_html ~pygmentize phrases
  | `OCaml x
  | `OCaml_rawtoplevel x ->
     (Pygments.pygmentize ~pygmentize `OCaml x >>| fun x -> [x])
  | `Other x ->
     (Pygments.pygmentize ~pygmentize:false `OCaml x >>| fun x -> [x])
  ) >>| fun l ->
  Html.div ~a:["class","highlight"] l


(******************************************************************************)
(* Main Operations                                                            *)
(******************************************************************************)
let eval_script lang ~filename =
  match lang with
  | `OCaml -> (
    Oloop.Script.of_file filename >>|? fun parts ->
    `OCaml parts
  )
  | `OCaml_toplevel -> (
    if String.is_suffix filename ~suffix:"async/main.topscript" then (
      Oloop.Script.of_file filename >>|? fun parts -> `OCaml_rawtoplevel parts
    )
    else (
      Oloop.Script.of_file filename >>=? fun script ->
      Sys.getcwd() >>= fun cwd ->
      Sys.chdir (Filename.dirname filename) >>= fun () ->
      Oloop.eval_script ~silent_directives:() ~short_paths:() script
      >>= function
      | Ok script ->
	 (Sys.chdir cwd >>| fun () -> Ok (`OCaml_toplevel script))
      | Error _ as e ->
	 (Sys.chdir cwd >>| fun () -> e)
    )
  )
  | `OCaml_rawtoplevel -> (
    Oloop.Script.of_file filename >>|? fun parts ->
    `OCaml_rawtoplevel parts
  )
  | _ -> (
    Reader.file_contents filename >>| fun x ->
    Ok (`Other x)
  )

let add_script t lang ~filename =
  let dir,file = filename in
  let filename = dir/file in
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
    add_script accum i.Import.data_code_language ~filename:(dir,i.Import.href)
  )

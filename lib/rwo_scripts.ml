open Core.Std
open Async.Std
module Html = Rwo_html
module Import = Rwo_import
let (/) = Filename.concat

type script = [
| `OCaml of Oloop.Script.t
| `OCaml_toplevel of Oloop.Script.Evaluated.t
| `OCaml_rawtoplevel of Oloop.Script.t
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
let phrases_to_html phrases =
  let buf = Buffer.create 1000 in
  let fmt = Format.formatter_of_buffer buf in
  let out_phrase_to_string x =
    !Toploop.print_out_phrase fmt x;
    Buffer.contents buf
  in
  List.map phrases ~f:(fun x ->
    let outcome = match x.Oloop.Script.Evaluated.outcome with
      | `Eval eval ->
         sprintf "%s\n%s\n"
           (out_phrase_to_string (Oloop.Outcome.result eval))
           (Oloop.Outcome.stdout eval)
      | `Uneval (_,msg) ->
         sprintf "%s\n" msg
    in
    sprintf "# %s\n%s\n" x.Oloop.Script.Evaluated.phrase outcome
  )
  |> String.concat ~sep:""
  |> fun x ->
     x
     |> String.substr_replace_all ~pattern:"<" ~with_:"&lt;"
     |> String.substr_replace_all ~pattern:">" ~with_:"&gt;"
     |> fun x -> Html.pre [`Data x]


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
      Oloop.Script.of_file filename >>=?
      Oloop.eval_script >>|? fun script -> `OCaml_toplevel script
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

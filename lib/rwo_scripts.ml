open Core.Std
open Async.Std
module Bash_script = Rwo_bash_script
module Html = Rwo_html
module Import = Rwo_import
module Lang = Rwo_lang
module Pygments = Rwo_pygments
let (/) = Filename.concat

type part = string

(* type oloop_script = Oloop.Script.t *)
(* type oloop_script_evaluated = Oloop.Script.Evaluated.t *)
(* type oloop_script_evaluated_phrase = Oloop.Script.Evaluated.phrase  *)
type oloop_script = SCRIPT
type oloop_script_evaluated = EVALUATED
type oloop_script_evaluated_phrase = PHRASE
type oloop_outcome = OUTCOME

let oloop_script_evaluated_phrase PHRASE = ""

let oloop_script_evaluated_outcome PHRASE
  : [ `Uneval of [`UNEVAL] * unit | `Eval of oloop_outcome ]
  = `Uneval (`UNEVAL, ())

let oloop_outcome_stdout OUTCOME = ""
let oloop_outcome_warnings OUTCOME : (Location.t * Warnings.t) list = []
let oloop_outcome_report_uneval ~msg_with_location:_ _ppf `UNEVAL = ()
let oloop_outcome_result OUTCOME = Outcometree.Ophr_signature []
let oloop_phrase_remove_underscore_names (x : Outcometree.out_phrase) = x

let oloop_script_of_file (_ : string) =
  Deferred.Or_error.return SCRIPT

let oloop_eval_script ~silent_directives:() ~short_paths:() SCRIPT =
  Deferred.Result.return EVALUATED


type script = [
| `OCaml of oloop_script
| `OCaml_toplevel of oloop_script_evaluated
| `OCaml_rawtoplevel of oloop_script
| `Other of string
]

type script_part = [
| `OCaml of string
| `OCaml_toplevel of oloop_script_evaluated_phrase list
| `OCaml_rawtoplevel of string
| `Other of string
]

type t = script String.Map.t

(******************************************************************************)
(* Map-style Operations                                                       *)
(******************************************************************************)
let empty = String.Map.empty

(*let of_script (parts:Oloop.Script.t) : (part * string) list =
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
      (filename,part) [%sexp_of: string * part] )
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
    if part = 0. then x else no_part_err()*)

let find_exn _ ?part:_ ~filename:_ =
  raise Not_found

let file_is_mem = Map.mem


(******************************************************************************)
(* Printers                                                                   *)
(******************************************************************************)
let phrases_to_html ?(pygmentize=false) phrases =

  let in_phrase (x:oloop_script_evaluated_phrase) : Html.item Deferred.t =
    match String.split (oloop_script_evaluated_phrase x) ~on:'\n' with
    | [] -> assert false
    | x::xs ->
      let x = sprintf "# %s" x in
      let phrase = String.concat ~sep:"\n  " (x::xs) in
      Pygments.pygmentize ~pygmentize `OCaml phrase
  in

  (* get warnings or errors *)
  let messages (x:oloop_script_evaluated_phrase) : Html.item option =
    (
      match oloop_script_evaluated_outcome x with
      | `Uneval (x,_) ->
        (
          try (
            oloop_outcome_report_uneval
              ~msg_with_location:true Format.str_formatter x;
            [Format.flush_str_formatter()]
          )
          with exn -> (
              Log.Global.error
                "Oloop.Outcome.report_uneval raised exception: %s"
                (Exn.to_string exn)
              ;
              ["Oloop error: unable to show correct OCaml output"]
            )
        )
      | `Eval e ->
        oloop_outcome_warnings e
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
    | l -> Some Html.(pre [`Data (String.concat l ~sep:"\n" |> Html.encode)])
  in

  let stdout (x:oloop_script_evaluated_phrase) : Html.item option =
    match oloop_script_evaluated_outcome x with
    | `Uneval _ -> None
    | `Eval e -> match oloop_outcome_stdout e with
      | "" -> None
      | x -> Some Html.(pre [`Data (Html.encode x)])
  in

  let out_phrase (x:oloop_script_evaluated_phrase)
    : Html.item option Deferred.t
    =
    match oloop_script_evaluated_outcome x with
    | `Uneval _ -> return None
    | `Eval e ->
      let buf = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buf in
      !Oprint.out_phrase fmt (
        oloop_outcome_result e |> oloop_phrase_remove_underscore_names
      );
      Buffer.contents buf
      |> Pygments.pygmentize ~add_attrs:["class","ge"] ~pygmentize `OCaml
      >>| Option.some
  in

  let phrase_to_html (x:oloop_script_evaluated_phrase) : Html.t Deferred.t =
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
  match (lang : Lang.t :> string) with
  | "ml" | "mli" | "mll" | "mly" -> (
    (* Hack: Oloop.Script.of_file intended only for ml files but
       happens to work for mli, mll, and mly files. *)
    oloop_script_of_file filename >>|? fun parts ->
    `OCaml parts
    )
  | "rawtopscript" -> (
    oloop_script_of_file filename >>|? fun parts ->
    `OCaml_rawtoplevel parts
  )
  | "topscript" -> (
    if String.is_suffix filename ~suffix:"async/main.topscript" then (
      oloop_script_of_file filename >>|? fun parts -> `OCaml_rawtoplevel parts
    )
    else (
      oloop_script_of_file filename >>=? fun script ->
      Sys.getcwd() >>= fun cwd ->
      Sys.chdir (Filename.dirname filename) >>= fun () ->
      oloop_eval_script ~silent_directives:() ~short_paths:() script
      >>= function
      | Ok script ->
	 (Sys.chdir cwd >>| fun () -> Ok (`OCaml_toplevel script))
      | Error _ as e ->
	 (Sys.chdir cwd >>| fun () -> e)
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
    add_script accum (Import.lang_of i |> ok_exn) ~filename:(dir,i.Import.href)
  )

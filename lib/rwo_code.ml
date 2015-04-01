open Core.Std
open Rwo_core2
open Async.Std
module Html = Rwo_html

type lang = [
  | `OCaml
  | `OCaml_toplevel
  | `OCaml_rawtoplevel
  | `Console
  | `JSON
  | `ATD
  | `Scheme
  | `OCaml_syntax
  | `C
  | `Bash
  | `CPP
  | `Java
  | `Ascii
  | `Gas
] with sexp

(* Map from file name to part number to value of type ['a]. In the
   code below we often refer to the outer map as [files] and the inner
   map as [parts].

   We use float keys for part numbers, thus relying on floating point
   equality. This should be okay given our usage of part numbers is
   fairly simple. *)
type 'a t = (lang * 'a Float.Map.t) String.Map.t

type phrase = {
  input : string;
  output : string;
  stdout : string;
  stderr : string;
} with sexp


(******************************************************************************)
(* Lang Operations                                                            *)
(******************************************************************************)
let lang_of_string = function
  | "ocaml"    -> Ok `OCaml
  | "ocamltop" -> Ok `OCaml_toplevel
  | "ocamlrawtop" -> Ok `OCaml_rawtoplevel
  | "console"  -> Ok `Console
  | "json"     -> Ok `JSON
  | "atd"      -> Ok `ATD
  | "scheme"   -> Ok `Scheme
  | "ocamlsyntax" -> Ok `OCaml_syntax
  | "java"     -> Ok `Java
  | "c"        -> Ok `C
  | "sh"       -> Ok `Bash
  | "cpp"      -> Ok `CPP
  | "ascii"    -> Ok `Ascii
  | "gas"      -> Ok `Gas
  | x          -> error "unknown code language" x sexp_of_string

let lang_to_string = function
  | `OCaml             -> "ocaml"
  | `OCaml_toplevel    -> "ocamltop"
  | `OCaml_rawtoplevel -> "ocamlrawtop"
  | `Console           -> "console"
  | `JSON              -> "json"
  | `ATD               -> "atd"
  | `Scheme            -> "scheme"
  | `OCaml_syntax      -> "ocamlsyntax"
  | `Java              -> "java"
  | `C                 -> "c"
  | `Bash              -> "sh"
  | `CPP               -> "cpp"
  | `Ascii             -> "ascii"
  | `Gas               -> "gas"

let lang_to_docbook_language lang = match lang with
  | `OCaml             -> Ok "ocaml"
  | `Console           -> Ok "console"
  | `JSON              -> Ok "json"
  | `ATD               -> Ok "ocaml"
  | `Scheme            -> Ok "scheme"
  | `Java              -> Ok "java"
  | `C                 -> Ok "c"
  | `Bash              -> Ok "bash"
  | `CPP               -> Ok "c"
  | `Gas               -> Ok "gas"
  | `OCaml_toplevel
  | `OCaml_rawtoplevel
  | `OCaml_syntax
  | `Ascii ->
    error "language not supported by docbook" lang sexp_of_lang

let lang_to_pygmentize_language lang = match lang with
  | `OCaml
  | `OCaml_toplevel
  | `OCaml_rawtoplevel
  | `ATD               -> Ok "ocaml"
  | `JSON              -> Ok "json"
  | `Scheme            -> Ok "scheme"
  | `Java              -> Ok "java"
  | `C                 -> Ok "c"
  | `Bash              -> Ok "bash"
  | `CPP               -> Ok "c"
  | `Gas               -> Ok "gas"
  | `Console
  | `OCaml_syntax
  | `Ascii ->
    error "we are not supporting this language for pygmentize" lang sexp_of_lang


(******************************************************************************)
(* Map-style Operations                                                       *)
(******************************************************************************)
let empty = String.Map.empty

let find files ?(part = 0.) ~file =
  match String.Map.find files file with
  | None -> None
  | Some (_,parts) -> Float.Map.find parts part

let find_exn files ?(part = 0.) ~file =
  match String.Map.find files file with
  | None -> ok_exn (error "no data for file" file sexp_of_string)
  | Some (_,parts) -> match Float.Map.find parts part with
    | None -> ok_exn (
      error "no data for requested part of file"
        (file,part) <:sexp_of< string * float >>
    )
    | Some x -> x

let files_langs_parts files =
  String.Map.fold files ~init:[] ~f:(fun ~key:file ~data:(lang,parts) accum ->
    accum@(Map.keys parts |> List.map ~f:(fun part -> (file,lang,part)))
  )

let file_is_mem = Map.mem

let lang_of_file files file =
  match Map.find files file with
  | None -> None
  | Some (lang,_) -> Some lang


(******************************************************************************)
(* Add Code Files                                                             *)
(******************************************************************************)
(** [line_to_part filename lang line] parses the part number from a
    line as appropriate for the syntax of [lang]. [None] is returned
    if the line does not indicate the start of a new part. [Error] is
    returned if the line does indicate start of a new part, but there
    is an error in the formatting. The [filename] is only for error
    messages. *)
let line_to_part filename lang line : float option Or_error.t =
  match lang with
  | `OCaml_toplevel
    -> (
      let lstripped = String.lstrip line in
      if String.is_prefix ~prefix:"#part " lstripped then (
        try Ok (Some (Scanf.sscanf lstripped "#part %f" ident))
        with _ ->
          error "invalid \"#part N\" line"
            (filename,line) <:sexp_of< string * string >>
      )
      else
        Ok None
    )
  | `OCaml
    -> (
      let lstripped = String.lstrip line in
      if String.is_prefix ~prefix:"(* part " lstripped then (
        try Ok (Some (Scanf.sscanf lstripped "(* part %f *)" ident))
        with _ ->
          error "invalid (* part N *) line"
            (filename,line) <:sexp_of< string * string >>
      )
      else
        Ok None
    )
  | _ ->
    error "splitting lang file into parts not supported"
      lang sexp_of_lang
;;

(* Implementation optimizes for [lang]s which aren't split into
   parts. The code for [`OCaml] could be run for all [lang]s and we
   could return [None] for the catch-all case of
   [line_to_part]. However, this would needlessly split and
   concatenate strings without altering them. *)
let split_parts_exn ~lang ~filename contents =
  match lang with
  | `Console
  | `JSON
  | `ATD
  | `Scheme
  | `OCaml_syntax
  | `C
  | `Bash
  | `CPP
  | `Java
  | `Ascii
  | `Gas
  | `OCaml_rawtoplevel
    ->
    [0.,contents]

  | `OCaml_toplevel
  | `OCaml -> (
    String.split ~on:'\n' contents
    |> List.fold
        ~init:(0., [ (0., Buffer.create 100) ])
        ~f:(fun (part,parts) line ->
          match ok_exn (line_to_part filename lang line) with
          | Some part ->
            (part, (part,(Buffer.create 100)) :: parts)
          | None ->
            (
              match List.Assoc.find parts part with
              | Some buf ->
                Buffer.add_string buf line;
                Buffer.add_char buf '\n';
                (part,parts)
              | None ->
                assert false
            )
        )
    |> snd
    |> List.map ~f:(fun (a,b) -> (a, String.strip (Buffer.contents b)))
    |> List.rev
  )
;;

let split_parts_of_file_exn ~lang ~filename =
  Reader.file_contents filename
  >>| split_parts_exn ~lang ~filename
;;

let add_file_exn t ~lang ~run file =
  run file >>|
  Float.Map.of_alist_exn >>| fun parts ->
  String.Map.add t ~key:file ~data:(lang,parts)
;;


(******************************************************************************)
(* OCaml language operations                                                  *)
(******************************************************************************)
let split_ocaml_toplevel_phrases_eol s =
  let rec loop (phrase,phrases) = function
    | [] -> (phrase,phrases)
    | line::lines ->
      let accum =
        if String.rstrip line |> String.is_suffix ~suffix:";;" then
          ([], (line::phrase)::phrases)
        else
          (line::phrase, phrases)
      in
      loop accum lines
  in
  let make_phrase l = String.concat ~sep:"\n" (List.rev l) in
  let phrase,phrases = loop ([],[]) (String.split_lines s) in
  match phrase with
  | _::_ ->
    error "OCaml toplevel phrase did not end with double semicolon"
      (make_phrase phrase) sexp_of_string
  | [] ->
    match phrases with
    | [] -> Or_error.error_string "empty OCaml toplevel phrase"
    | _ -> Ok (List.map (List.rev phrases) ~f:make_phrase)
;;

let split_ocaml_toplevel_phrases_anywhere s =
  let open Result.Monad_infix in
  let s = String.rstrip s in
  let indexes =
    String.substr_index_all s ~may_overlap:false ~pattern:";;"
    |> fun l -> -2::l
  in
  (
    match indexes with
    | [] -> assert false
    | _::[] ->
      error "double semicolon not found in OCaml toplevel phrase"
        s sexp_of_string
    | _ ->
      let rec loop accum = function
        | [] -> accum
        | _::[] -> accum
        | i::(j::_ as indexes) ->
          loop ((i+2,j+2)::accum) indexes
      in
      Ok (loop [] indexes)
  )
  >>= fun ranges ->
  match ranges with
  | [] -> assert false
  | (_,last)::_ ->
    if last < String.length s then
      error "OCaml toplevel phrase doesn't end with ;;"
        s sexp_of_string
    else
      List.rev ranges
      |> List.map ~f:(fun (i,j) -> String.slice s i j)
      |> fun x -> Ok x
;;


let split_ocaml_toplevel_phrases where s = match where with
  | `Eol -> split_ocaml_toplevel_phrases_eol s
  | `Anywhere -> split_ocaml_toplevel_phrases_anywhere s
;;

(** Parse given file, whose basename must of the form foo.N.sexp,
    where N is a part number. Returns the part number and the file
    contents parsed as a list of [phrase]s. Return None if file
    extension is not ".sexp". Raises exception in case of any
    error. *)
let parse_ocaml_toplevel_phrases file
    : (float * phrase list) option Deferred.t
    =
  let base = Filename.basename file in
  match Filename.split_extension base with
  | (base, Some "sexp") -> (
    match String.split ~on:'.' base with
    | _::(_::_ as part_pieces) ->
      let part = String.concat part_pieces ~sep:"." |> Float.of_string in
      Reader.file_contents file >>| fun contents ->
      let phrases = Sexp.of_string contents |> <:of_sexp< phrase list >> in
      Some (part, phrases)
    | [] -> assert false
    | _ ->
      ok_exn (error "unexpected filename output by run_core_toplevel"
                file sexp_of_string)
  )
  | (_, Some _) -> return None
  | (_, None) -> return None
;;

let run_ocaml_toplevel_file_exn ?(repo_root=".") file =
  let (/) = Filename.concat in

  let to_abs cwd file =
    if Filename.is_relative file
    then cwd/file
    else file
  in

  let f rwo_runtop temp_dir =
    (* run [file] through toplevel *)
    let runtop_cmd =
      sprintf "%s -o %s %s" rwo_runtop temp_dir (Filename.basename file)
    in
    printf "%s\n" runtop_cmd;
    Sys.command_exn runtop_cmd >>= fun () ->

    (* parse files output in previous step *)
    Sys.readdir temp_dir >>= fun files ->
    return (Array.to_list files) >>= fun files ->
    return (List.map files ~f:(Filename.concat temp_dir)) >>= fun files ->
    Deferred.List.filter_map files ~f:parse_ocaml_toplevel_phrases
    >>= fun alist ->

    (* delete files generated above *)
    Deferred.List.iter files ~f:Unix.unlink >>= fun () ->
    Unix.rmdir temp_dir >>= fun () ->

    return alist
  in

  Sys.getcwd() >>= fun cwd ->
  let repo_root = to_abs cwd repo_root in
  let file = to_abs cwd file in
  let build_dir = repo_root/"_build" in
  let rwo_runtop = build_dir/"app"/"rwo_runtop" in
  let work_dir = Filename.dirname file in

  try_with (fun () ->
    let temp_dir = Filename.temp_dir ~in_dir:build_dir "" "" in
    Sys.chdir work_dir >>= fun () ->
    f rwo_runtop temp_dir
  ) >>= fun ans ->
  Sys.chdir cwd >>| fun () ->
  Result.ok_exn ans
;;

let run_file_exn ?(repo_root=".") file ~lang = match lang with
  | `OCaml_toplevel ->
    run_ocaml_toplevel_file_exn ~repo_root file
  | _ ->
    (
      split_parts_of_file_exn ~lang ~filename:file >>|
      List.map ~f:(fun (part,input) ->
        part, [{input; output=""; stdout=""; stderr=""}]
      )
    )


(******************************************************************************)
(* Printers                                                                   *)
(******************************************************************************)
let pygmentize lang contents =
  match lang_to_pygmentize_language lang with
  | Error _ -> return (Html.(pre [`Data contents]))
  | Ok lang ->
    Process.create ~prog:"pygmentize" ~args:["-l"; lang; "-f"; "html"] ()
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
          Html.pre (Html.of_string stdout)
    )

let phrases_to_html ?(run_pygmentize=false) lang xs =
  (
    match lang with
    | `OCaml_toplevel ->
      List.map xs ~f:(fun x ->
        sprintf "# %s\n%s%s%s" (String.strip x.input) x.stdout x.stderr x.output
      )
    | _ ->
      List.map xs ~f:(fun x ->
        sprintf "%s\n%s%s%s" x.input x.output x.stdout x.stderr
      )
  )
  |> String.concat ~sep:""
  |> fun x ->
    if run_pygmentize
    then pygmentize lang x
    else return (
      x
      |> String.substr_replace_all ~pattern:"<" ~with_:"&lt;"
      |> String.substr_replace_all ~pattern:">" ~with_:"&gt;"
      |> fun x -> Html.(pre [`Data x])
    )

(* TODO: implement with Async *)
let run_through_pygmentize lang contents =
  let open Core.Std in
  let lang = ok_exn (lang_to_pygmentize_language lang) in
  let ic,oc = Unix.open_process (sprintf "pygmentize -l %s -f html" lang) in
  Out_channel.output_string oc contents;
  Out_channel.close oc;
  let html = Cow.Html.of_string (In_channel.input_all ic) in
  match html with
  |`El ((("","div"),[(("","class"),"highlight")]),[`El ((("","pre"),[]),data)]) :: _ ->
    Ok <:html<<div class="highlight">$data$</div>&>>
  |_ ->
    Or_error.error_string
      "unexpected pygments output: not <div class=highlight><pre>..."

let concat_toplevel_phrases lines =
  let open Result.Monad_infix in
  let combine l = String.concat ~sep:"\n" (List.rev l) in
  List.fold_left lines ~init:(`output ([],[])) ~f:(fun state line ->
      match state with
      |`phrase (res,acc) -> begin
          if String.is_suffix ~suffix:";;" line then
            let res = combine (line::acc) :: res in
            `output (res,[])
          else `phrase (res,line::acc)
        end
      |`output (res,acc) -> begin
          if String.is_prefix ~prefix:"#" line then begin
            let res = combine acc :: res in
            if String.is_suffix ~suffix:";;" line then
              `output ((line::res),[])
            else `phrase (res,[line])
          end else `output (res,line::acc)
        end
    )
  |> (function
      |`phrase _ -> Or_error.error_string "unterminated phrase"
      |`output (res,acc) -> Ok (List.rev (combine acc :: res)))
  >>| List.filter ~f:(function "" -> false | _ -> true)

let wrap_in_pretty_box ~part lang file (buf:Cow.Xml.t) =
  let repourl = sprintf "http://github.com/realworldocaml/examples/" in
  let fileurl =
    sprintf
      "http://github.com/realworldocaml/examples/blob/master/code/%s"
      file
  in
  let string_of_float = Float.to_string in
  let part =
    match part with
    | 0. -> []
    | part -> <:html<, continued (part $flo:part$)>>
  in
  let info = <:html<$str:lang$ &lowast; <a href=$str:fileurl$>$str:file$</a> $part$ &lowast; <a href=$str:repourl$>all code</a>&>> in
  <:html<<div class="rwocode"><pre><code>$buf$</code></pre><div class="rwocodeinfo">$info$</div></div>&>>

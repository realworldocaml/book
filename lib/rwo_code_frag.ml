open Core.Std
open Rwo_core2
open Async.Std

type typ = [
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

type t = {
  typ: string;
  name: string;
  part: int with default(0);
  header: bool with default(true)
} with sexp

let typ_of_string = function
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
  | x          -> error "unknown fragment type" x sexp_of_string

let typ_to_string (t:typ) =
  match t with
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

let file_of_t ~ext t =
  sprintf "code/_build/%s.%d.%s" t.name t.part ext

let read ~ext t =
  Reader.file_contents (file_of_t ~ext t)

(* Convert typ to a Docbook language tag *)
let typ_to_docbook_language (t:typ) =
  match t with
  | `OCaml             -> "ocaml"
  | `OCaml_toplevel    -> ""
  | `OCaml_rawtoplevel -> ""
  | `Console           -> "console"
  | `JSON              -> "json"
  | `ATD               -> "ocaml"
  | `Scheme            -> "scheme"
  | `OCaml_syntax      -> ""
  | `Java              -> "java"
  | `C                 -> "c"
  | `Bash              -> "bash"
  | `CPP               -> "c"
  | `Ascii             -> ""
  | `Gas               -> "gas"

let of_string s =
  try
    Ok (String.strip s |> Sexp.of_string |> t_of_sexp)
  with exn ->
    error "cannot parse sexp as Code_frag.t"
      (s, exn) <:sexp_of< string * exn >>

let extract_ocaml_parts filename buf =
  let open Result.Monad_infix in
  let ans = Result.List.fold
    (String.split ~on:'\n' buf)
    ~init:(0, [ (0, Buffer.create 100) ])
    ~f:(fun (part,parts) line ->
      let lstripped = String.lstrip line in
      if String.is_prefix ~prefix:"(* part " lstripped then
        let part = Scanf.sscanf lstripped "(* part %d *)" ident in
        Ok (part, (part,(Buffer.create 100)) :: parts)
      else (
        match List.Assoc.find parts part with
        | Some buf ->
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          Ok (part,parts)
        | None ->
          error "expected part number not found"
            (part,filename,buf)
          <:sexp_of< int * string * string >>
      )
    )
  in
  ans >>| snd >>|
  List.map ~f:(fun (a,b) -> (a, String.strip (Buffer.contents b)))

let extract_ocaml_part filename part buf =
  let open Result.Monad_infix in
  extract_ocaml_parts filename buf >>= fun parts ->
  match List.Assoc.find parts part with
  | None ->
    error "part number not found"
      (part,filename,buf) <:sexp_of< int * string * string >>
  | Some buf -> Ok buf

(* TODO: implement with Async *)
let run_through_pygmentize lang contents =
  let open Core.Std in
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

let wrap_in_pretty_box ~part typ file (buf:Cow.Xml.t) =
  let repourl = sprintf "http://github.com/realworldocaml/examples/" in
  let fileurl =
    sprintf
      "http://github.com/realworldocaml/examples/blob/master/code/%s"
      file
  in
  let part =
    match part with
    | 0 -> []
    | part -> <:html<, continued (part $int:part$)>>
  in
  let info = <:html<$str:typ$ &lowast; <a href=$str:fileurl$>$str:file$</a> $part$ &lowast; <a href=$str:repourl$>all code</a>&>> in
  <:html<<div class="rwocode"><pre><code>$buf$</code></pre><div class="rwocodeinfo">$info$</div></div>&>>

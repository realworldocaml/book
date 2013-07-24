(* A fragment description, intended to be embedded in the book as
   ```frag
   ((typ xxx)(name xxx)(part 1)(header false))
   ```
   where the (part X) defaults to 0 and (header) defaults to true
   If (part X) is specified, then there will be '#' preprocessor directives
   in the [name] file.

   The [name] file should be under the `code/` subdirectory
*)

open Core.Std

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
]

type t = {
  typ: string;
  name: string;
  part: int with default(0);
                 header: bool with default(true)
} with sexp

let typ_of_string s : typ =
  match s with
  | "ocaml"    -> `OCaml
  | "ocamltop" -> `OCaml_toplevel
  | "ocamlrawtop" -> `OCaml_rawtoplevel
  | "console"  -> `Console
  | "json"     -> `JSON
  | "atd"      -> `ATD
  | "scheme"   -> `Scheme
  | "ocamlsyntax" -> `OCaml_syntax
  | "java"     -> `Java
  | "c"        -> `C
  | "sh"       -> `Bash
  | "cpp"      -> `CPP
  | "ascii"    -> `Ascii
  | "gas"      -> `Gas
  | x          -> raise (Failure ("Unknown fragment type " ^ x))

let of_string s =
  try
    String.strip s |> Sexp.of_string |> t_of_sexp
  with exn ->
    eprintf "ERR: %s\n while parsing: %s\n%!"
      (Exn.to_string exn) s; raise exn

(** Hunt through OCaml code and split out any comments that
    are of the form (* part X *)
*)
let extract_all_ocaml_parts filename buf =
  let rec iter part parts =
    function
    |line::lines when String.is_prefix ~prefix:"(* part " line ->
      let part = Caml.Scanf.sscanf line "(* part %d *)" (fun p -> p) in
      let parts = (part, (Buffer.create 100)) :: parts in
      iter part parts lines
    |line::lines -> begin
        match List.Assoc.find parts part with
        | Some buf ->
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          iter part parts lines
        | None ->
          eprintf "no part %d in %s\n\n%s%!" part filename buf; 
          exit (-1)
      end
    |[] -> List.map parts ~f:(fun (a,b) -> (a, String.strip (Buffer.contents b)))
  in
  let parts = [ (0, Buffer.create 100) ] in
  iter 0 parts (String.split ~on:'\n' buf)

let extract_ocaml_part filename part buf =
  let parts = extract_all_ocaml_parts filename buf in
  match List.Assoc.find parts part with
  | None -> eprintf "no part %d found in %s\n\n%s" part filename buf; exit (-1)
  | Some buf -> buf

let run_through_pygmentize lang contents =
  let ic,oc = Unix.open_process (sprintf "pygmentize -l %s -f html" lang) in
  Out_channel.output_string oc contents;
  Out_channel.close oc;
  let html = Cow.Html.of_string (In_channel.input_all ic) in
  match html with
  |`El ((("","div"),[(("","class"),"highlight")]),[`El ((("","pre"),[]),data)]) :: _ ->
    <:html<<div class="highlight">$data$</div>&>>
  |_ -> raise (Failure "unexpected pygments output: not <div class=highlight><pre>...")

(* concat toplevel phrases so that each toplevel phrase always starts with a ;; *)
let concat_toplevel_phrases lines =
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
      |`phrase _ -> failwith "unterminated phrase"
      |`output (res,acc) -> List.rev (combine acc :: res))
  |> List.filter ~f:(function |"" -> false |_ -> true)

let wrap_in_pretty_box ~part typ file (buf:Cow.Xml.t) =
  let repourl = sprintf "http://github.com/realworldocaml/code/" in
  let fileurl = sprintf "http://github.com/realworldocaml/code/TODO/%s" file in
  let part =
    match part with
    | 0 -> []
    | part -> <:html<, continued (part $int:part$)>>
  in
  let info = <:html<$str:typ$ &lowast; <a href=$str:fileurl$>$str:file$</a> $part$ &lowast; <a href=$str:repourl$>all code</a>&>> in
  <:html<<div class="rwocode"><pre><code>$buf$</code></pre><div class="rwocodeinfo">$info$</div></div>&>>

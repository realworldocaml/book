(* Syntax highlight some code, either through Pygments
 * or Cow's OCaml syntax highlighting *)
open Core.Std
open Xml_tree
open Code_frag

let build_dir = ref ""
let ofile_html file part = sprintf "%s/%s.%d.html" !build_dir file part
let ofile_md file part   = sprintf "%s/%s.%d.md"   !build_dir file part

(* Run a buffer through Pygments to colorize it *)
let pygmentize lang file contents =
  (* The contents of <pre> are just Data tags, so filter them through
     Pygments *)
  let data = run_through_pygmentize lang contents in
  let typ =
    match lang with
    | "ocaml" -> "OCaml Source Code"
    | "json" -> "JSON"
    | "console" -> "Terminal"
    | unknown -> unknown
  in
  let data = wrap_in_pretty_box ~part:0 typ file [data] |> Cow.Html.to_string in
  Out_channel.write_all (ofile_html file 0) ~data;
  Out_channel.write_all (ofile_md file 0) ~data:contents

let cow file contents =
  (* Break the OCaml code into parts *)
  Code_frag.extract_all_ocaml_parts file contents
  |> List.iter ~f:(fun (part,buf) ->
      let code = Cow.Code.ocaml_fragment buf in
      let html = wrap_in_pretty_box ~part "OCaml" file [code] in
      let data = Cow.Html.to_string html in
      Out_channel.write_all (ofile_html file part) ~data;
      Out_channel.write_all (ofile_md file part) ~data:buf;
    )

let console file = 
  (* Run lines starting with $ through pygments, pass rest through *)
  let olines = List.rev (In_channel.with_file file ~f:(
    In_channel.fold_lines ~init:[] ~f:(fun acc line ->
      if String.is_prefix ~prefix:"$ " line then
        (run_through_pygmentize "console" line) :: acc
      else <:html<<div class="rwocodeout">$str:line$</div>&>> :: acc
    ))) in
  let buf =
    wrap_in_pretty_box ~part:0 "Terminal" file olines
    |> Cow.Html.to_string in
  Out_channel.write_all (ofile_html file 0) ~data:buf;
  Out_channel.write_all (ofile_md file 0) ~data:(In_channel.read_all file)

let do_highlight build_dir' use_cow use_pygments use_console file () =
  build_dir := build_dir';
  let buf = In_channel.read_all file in
  if use_cow then
    cow file buf
  else if use_console then
    console file 
  else match use_pygments with
   | Some lang -> pygmentize lang file buf
   | None -> raise (Failure "No flags specified")

let () =
  Command.basic
    ~summary:"Syntax highlight code to HTML and Markdown"
    Command.Spec.(empty
                  +> flag "-builddir" (optional_with_default "." string)
                      ~doc:"dir Prepend directory to output files"
                  +> flag "-cow" no_arg 
                      ~doc:" Filter OCaml through COW, extracting only part %d "
                  +> flag "-pygments" (optional string)
                      ~doc:"lang Filter through Pygments with given [lang]"
                  +> flag "-console" no_arg
                      ~doc:" Filter shell script output into HTML"
                  +> anon ("filename" %: file)
                 )
    do_highlight
  |> Command.run

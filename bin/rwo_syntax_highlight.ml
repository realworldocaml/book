(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(* Syntax highlight some code, either through Pygments
 * or Cow's OCaml syntax highlighting *)
open Core.Std
open Xml_tree
open Code_frag

let build_dir = ref ""
let ofile_html file part = sprintf "%s/%s.%d.html" !build_dir file part
let ofile_md file part   = sprintf "%s/%s.%d.md"   !build_dir file part
let ofile_xml file part  = sprintf "%s/%s.%d.xml"  !build_dir file part

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
    | "scheme" -> "S-expression"
    | "html" -> "Syntax"
    | "java" -> "Java"
    | "c" -> "C"
    | "cmd" -> "Shell script"
    | "cpp" -> "C++"
    | "gas" -> "Assembly Language"
    | unknown -> unknown
  in
  let data_html = wrap_in_pretty_box ~part:0 typ file data |> Cow.Html.to_string in
  Out_channel.write_all (ofile_html file 0) ~data:data_html;
  Out_channel.write_all (ofile_md file 0) ~data:contents;
  let data = wrap_in_docbook_box ~part:0 typ file <:xml<$str:contents$>> in
  Out_channel.write_all (ofile_xml file 0) ~data

let raw lang file contents =
  let data = <:html<<pre>$str:contents$</pre>&>> in
  let data = wrap_in_pretty_box ~part:0 lang file data |> Cow.Html.to_string in
  Out_channel.write_all (ofile_html file 0) ~data;
  Out_channel.write_all (ofile_md file 0) ~data:contents;
  let data = wrap_in_docbook_box ~part:0 lang file <:html<$str:contents$>> in
  Out_channel.write_all (ofile_xml file 0) ~data
  
let cow file contents =
  (* Break the OCaml code into parts *)
  Code_frag.extract_all_ocaml_parts file contents
  |> List.iter ~f:(fun (part,buf) ->
      let code = Cow.Code.ocaml_fragment buf in
      let html = wrap_in_pretty_box ~part "OCaml" file code in
      let data = Cow.Html.to_string html in
      Out_channel.write_all (ofile_html file part) ~data;
      Out_channel.write_all (ofile_md file part) ~data:buf;
      let data = wrap_in_docbook_box ~part "OCaml" file <:xml<$str:buf$>> in
      Out_channel.write_all (ofile_xml file part) ~data
    )

let rawscript file =
  (* Run lines starting with # through Cow, pass rest through *)
  Code_frag.concat_toplevel_phrases (In_channel.read_lines file)
  |> List.map ~f:(fun line ->
    let line = if line = "" then " " else line in
    if String.is_suffix ~suffix:";;" line then
      Cow.Code.ocaml_fragment line
    else
      <:html<<div class="rwocodeout">$str:line$</div>&>>)
  |> fun olines_html ->
  Code_frag.concat_toplevel_phrases (In_channel.read_lines file)
  |> List.map ~f:(fun line ->
    let line = if line = "" then " " else line in
    if String.is_suffix ~suffix:";;" line then
      <:xml<<userinput>$str:line$</userinput>
>>
    else
      <:xml<<computeroutput>$str:line$</computeroutput>
>>)
  |> fun olines_xml ->
  let buf =
    wrap_in_pretty_box ~part:0 "OCaml toplevel" file (List.concat olines_html)
    |> Cow.Html.to_string in
  Out_channel.write_all (ofile_html file 0) ~data:buf;
  Out_channel.write_all (ofile_md file 0) ~data:(In_channel.read_all file);
  let data = wrap_in_docbook_box ~part:0 "OCaml Utop" file (List.concat olines_xml) in
  Out_channel.write_all (ofile_xml file 0) ~data

let console file = 
  (* Run lines starting with $ through pygments, pass rest through *)
  let olines = List.rev (In_channel.with_file file ~f:(
    In_channel.fold_lines ~init:[] ~f:(fun acc line ->
      let line = if line = "" then " " else line in
      if String.is_prefix ~prefix:"$ " line then
        (run_through_pygmentize "console" line) :: acc
      else (<:html<<div class="rwocodeout">$str:line$</div>&>>) :: acc
    ))) in
  let olines_xml = List.rev (In_channel.with_file file ~f:(
    In_channel.fold_lines ~init:[] ~f:(fun acc line ->
      let line = if line = "" then " " else line in
      if String.is_prefix ~prefix:"$ " line then (
        let rest = String.sub line ~pos:2 ~len:(String.length line - 2) in
        let dollar = "$" in
        (<:xml<<prompt>$str:dollar$ </prompt><userinput>$str:rest$</userinput>
>>) :: acc
      ) else (<:xml<<computeroutput>$str:line$</computeroutput>
>>) :: acc
    ))) in
  let buf =
    wrap_in_pretty_box ~part:0 "Terminal" file (List.concat olines)
    |> Cow.Html.to_string in
  Out_channel.write_all (ofile_html file 0) ~data:buf;
  Out_channel.write_all (ofile_md file 0) ~data:(In_channel.read_all file);
  let data = wrap_in_docbook_box ~part:0 "Terminal" file (List.concat olines_xml) in
  Out_channel.write_all (ofile_xml file 0) ~data

let do_highlight build_dir' use_cow use_rawscript use_pygments use_raw use_console file () =
  build_dir := build_dir';
  let buf = In_channel.read_all file in
  if use_cow then
    cow file buf
  else if use_console then
    console file 
  else if use_rawscript then
    rawscript file
  else match use_pygments with
   | Some lang -> pygmentize lang file buf
   | None -> begin
      match use_raw with
      | Some lang -> raw lang file buf
      | None -> failwith "no flags"
   end

let () =
  Command.basic
    ~summary:"Syntax highlight code to HTML and Markdown"
    Command.Spec.(empty
                  +> flag "-builddir" (optional_with_default "." string)
                      ~doc:"dir Prepend directory to output files"
                  +> flag "-cow" no_arg 
                      ~doc:" Filter OCaml through COW, extracting only part %d "
                  +> flag "-rawscript" no_arg 
                      ~doc:" Filter OCaml toplevel through COW"
                  +> flag "-pygments" (optional string)
                      ~doc:"lang Filter through Pygments with given [lang]"
                  +> flag "-raw" (optional string)
                      ~doc:"lang Wrap raw file with given [lang]"
                  +> flag "-console" no_arg
                      ~doc:" Filter shell script output into HTML"
                  +> anon ("filename" %: file)
                 )
    do_highlight
  |> Command.run

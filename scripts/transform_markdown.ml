open Core.Std
open Code_frag

(* Convert an input file into parts, vi *)
(* Given an sexp of [t], output the Markdown *)
let output_t_as_markdown s =
  let t = of_string s in
  let typ = typ_of_string t.typ in
  let buf = In_channel.read_all (sprintf "code/_build/%s.%d.md" t.name t.part) in
  match typ with
  | `OCaml ->
    printf "```ocaml\n";
    if t.header then printf "(* %s %s *)\n" t.name
        (match t.part with
         | 0 -> ""
         | part -> sprintf "(starting from part %d)" part);
    printf "%s\n```\n" buf
  | `OCaml_toplevel | `OCaml_rawtoplevel ->
    printf "```ocaml\n";
    if t.header then 
      (match t.part with
       | 0 -> printf "# script %s\n$ utop\n" t.name
       | part -> printf "...part %d of %s\n" part t.name);
    printf "%s\n```\n" buf
  | `Bash ->
    printf "```bash\n#!/bin/sh\n# %s\n" t.name;
    printf "%s\n```\n" buf
  | `OCaml_syntax ->
    printf "```html\n";
    printf "%s\n```\n" buf
  | `Scheme ->
    printf "```scheme\n";
    if t.header then printf ";; %s.scm\n" t.name;
    printf "%s\n```\n" buf
  | `Console -> 
    printf "```console\n";
    if t.header then printf "# running %s.sh\n" t.name;
    printf "%s\n```\n" buf
  | `JSON ->
    printf "```json\n";
    printf "%s\n```\n" buf
  | `CPP ->
    printf "```cpp\n";
    printf "%s\n```\n" buf
  | `C ->
    printf "```c\n";
    printf "%s\n```\n" buf
  | `Java ->
    printf "```java\n";
    printf "%s\n```\n" buf
  | `ATD ->
    printf "```\n";
    if t.header then printf "(* %s *)\n" t.name;
    printf "%s\n```\n" buf

(* Filter a Markdown file and map [frag] constructs into code *)
let filter_markdown = 
  let rec filter state =
    match In_channel.(input_line stdin) with 
    | None -> ()
    | Some line -> begin
        match state,line with
        |`searching, "```frag"   -> filter (`found_frag (Buffer.create 1024))
        |`searching, line        -> print_endline line; filter `searching
        |`found_frag buf , "```" -> output_t_as_markdown (Buffer.contents buf); filter `searching
        |`found_frag buf , line  -> Buffer.add_string buf line; filter (`found_frag buf)
      end
  in
  filter `searching

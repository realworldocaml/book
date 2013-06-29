open Core.Std

(* A fragment description, intended to be embedded in the book as
   ```frag
   ((typ xxx)(name xxx)(part 1)(header false))
   ```
   where the (part X) defaults to 0 and (header) defaults to true
   If (part X) is specified, then there will be '#' preprocessor directives
   in the [name] file.

   The [name] file should be under the `code/` subdirectory
*)
type t = {
  typ: string;
  name: string;
  part: int with default(0);
                 header: bool with default(true)
} with sexp

let extract_ocaml_part filename part buf =
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
    |[] -> parts
  in
  let parts = [ (0, Buffer.create 100) ] in
  let parts = iter 0 parts (String.split ~on:'\n' buf) in
  match List.Assoc.find parts part with
  | None -> eprintf "no part %d found in %s\n\n%s" part filename buf; exit (-1)
  | Some buf -> Buffer.contents buf

(* Convert an input file into parts, vi *)
(* Given an sexp of [t], output the Markdown *)
let output_t_as_markdown s =
  let t =
    try
      String.rstrip s |> Sexp.of_string |> t_of_sexp
    with exn ->
      eprintf "ERR: %s\n while parsing: %s\n%!"
        (Exn.to_string exn) s; raise exn
  in
  let buf =
    match t.typ with
    | "ocaml" ->
      let buf = In_channel.read_all (sprintf "code/%s" t.name) in
      printf "```ocaml\n";
      if t.header then printf "(* %s %s *)\n" t.name
        (match t.part with
         | 0 -> ""
         | part -> sprintf "(starting from part %d)" part);
      extract_ocaml_part t.name t.part buf
    | "ocamltop" ->
      printf "```ocaml\n";
      if t.header then 
        (match t.part with
         | 0 -> printf "# script %s\n$ utop\n" t.name
         | part -> printf "...part %d of %s\n" part t.name);
      In_channel.read_all (sprintf "code/%s.%d.out" (Filename.chop_extension t.name) t.part)
    | "console" -> 
      printf "```console\n";
      let basename = Filename.chop_extension t.name in
      if t.header then printf "# running %s.sh\n" basename;
      In_channel.read_all (sprintf "code/%s" t.name)
    | "json" ->
      printf "```json\n";
      In_channel.read_all (sprintf "code/%s" t.name)
    | "atd" ->
      printf "```\n";
      if t.header then printf "(* %s *)\n" t.name;
      In_channel.read_all (sprintf "code/%s" t.name)
    | _ ->
      printf "```\n";
      In_channel.read_all (sprintf "code/%s" t.name)
  in
  printf "%s\n```\n" (String.rstrip buf)

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

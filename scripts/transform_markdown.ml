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
  (match t.typ with
   | "ocaml" ->
       printf "```ocaml\n";
       if t.header then printf "(* %s *)\n" t.name
   | "ocamltop" ->
       printf "```ocaml\n";
       if t.header then printf "# script %s\n$ utop\n" t.name
   | "console" -> 
       printf "```console\n";
       let basename = Filename.chop_extension t.name in
       if t.header then printf "# running %s.sh\n" basename
   | "json" ->
       printf "```json\n"
   | _ ->
       printf "```\n"
  );
  printf "%s```\n" (In_channel.read_all (sprintf "code/%s" t.name))

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

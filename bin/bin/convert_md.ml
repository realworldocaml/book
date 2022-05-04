let pp_section ppf (n, title) =
  let pp_n ppf () = Fmt.string ppf (String.make n '#') in
  Fmt.pf ppf "%a %s\n" pp_n () title

let pp_list pp = Fmt.(list ~sep:(any "") pp)

let pp_html ppf s =
  let add = function
    | '"' -> Fmt.string ppf "&quot;"
    | '&' -> Fmt.string ppf "&amp;"
    | '<' -> Fmt.string ppf "&lt;"
    | '>' -> Fmt.string ppf "&gt;"
    | c   -> Fmt.char ppf c
  in
  String.iter add s

let pp_output ppf = function
  |`Output s -> Fmt.pf ppf ">%a\n" pp_html s
  |`Ellipsis -> Fmt.pf ppf "...\n"

 let pp_html_line ppf l = Fmt.pf ppf "%a\n" pp_html l
 let pp_md_line ppf l = Fmt.pf ppf "%s\n" l

 let pp_toplevel ppf (t:Mdx.Toplevel.t) =
  Fmt.pf ppf "%a%a" (pp_list pp_html_line) t.command (pp_list pp_output) t.output

let pp_toplevel_block (b: Mdx.Block.t) ppf =
  let ts =
    Mdx.Toplevel.of_lines
      ~syntax:Normal
      ~loc:b.loc
      b.contents
  in
  pp_list pp_toplevel ppf ts

let pp_contents (t:Mdx.Block.t) ppf =
  Fmt.(list ~sep:(any "\n") pp_html) ppf t.contents

let pp_markdown_output ppf = function
  |`Output s -> Fmt.pf ppf "((:%s::)\n" s
  |`Ellipsis -> Fmt.pf ppf "...\n"

let pp_markdown_toplevel ppf (t:Mdx.Toplevel.t) =
  let cmds =
    match t.command with
    | hd::tl -> ("# " ^ hd) :: (List.map (fun l -> "  " ^ l) tl)
    | [] -> [] in
  Fmt.pf ppf "%a%a" (pp_list pp_md_line) cmds (pp_list pp_markdown_output) t.output

let pp_markdown_toplevel_block (b: Mdx.Block.t) ppf =
  let ts =
    Mdx.Toplevel.of_lines
      ~syntax:Normal
      ~loc:b.loc
      b.contents
  in
  pp_list pp_markdown_toplevel ppf ts

let pp_cram ppf (t:Mdx.Cram.t) =
  let pp_exit ppf = match t.exit_code with
    | 0 -> ()
    | i -> Fmt.pf ppf "[%d]" i
  in
  Fmt.pf ppf "%a%a%t"
    (pp_list pp_html_line) t.command
    (pp_list pp_output) t.output pp_exit

let pp_cram_block content ppf =
  pp_list pp_cram ppf (snd (Mdx.Cram.of_lines ~syntax:Mdx.Syntax.Normal ~loc:Location.none content))

let header_to_string = Fmt.str "%a" Mdx.Block.Header.pp 

let pp_block_html ppf (b:Mdx.Block.t) =
  let lang, pp_code, attrs = match b.value with
    | Toplevel _ -> Some "ocaml", pp_toplevel_block b, [
        ("class"             , "command-line");
        ("data-prompt"       , "#");
        ("data-filter-output", ">");
      ]
    | Include {file_kind = Fk_ocaml _; _}
    | OCaml _ -> Some "ocaml", pp_contents b, []
    | Cram _ -> Some "bash" , pp_cram_block b.contents, [
        ("class"             , "command-line");
        ("data-user"         , "fun");
        ("data-host"         , "lama");
        ("data-filter-output", ">");
      ]
    | Include {file_kind = Fk_other {header}; _}
    | Raw {header} ->
      Option.map header_to_string header, pp_contents b, []
  in
  let pp_attr ppf (k, v) = Fmt.pf ppf "%s=%S" k v in
  let pp_lang ppf () = match lang with
    | None   -> Fmt.pf ppf " class=\"language-clike\""
    | Some l -> Fmt.pf ppf " class=\"language-%s\"" l
  in
  let pp_attrs ppf () = match attrs with
    | [] -> ()
    | _  -> Fmt.pf ppf " %a" Fmt.(list ~sep:(any " ") pp_attr) attrs
  in
  Fmt.pf ppf "<div class=\"highlight\"><pre%a><code%a>%t</code></pre></div>"
    pp_attrs () pp_lang () pp_code

let pp_block_latex ppf (b:Mdx.Block.t) =
  let lang = match b.value with
    | Toplevel _
    | Include {file_kind = Fk_ocaml _; _}
    | OCaml _ -> Some "ocaml"
    | Cram _ -> Some "bash"
    | Include {file_kind = Fk_other {header}; _}
    | Raw {header} -> Option.map header_to_string header
  in
  let pp_code = (fun ppf -> Fmt.(list ~sep:(any "\n") string) ppf b.contents) in
  let lang = match lang with
    | None   -> "shell"
    | Some l -> l
  in
  Fmt.pf ppf "```%s\n%t\n```"
    lang pp_code

let pp_block_md ppf (b:Mdx.Block.t) =
  let pp_code = (fun ppf -> Fmt.(list ~sep:(any "\n") string) ppf b.contents) in
  let lang, pp_code = match b.value with
    | Toplevel _ -> Some "ocaml", (pp_markdown_toplevel_block b)
    | Include {file_kind = Fk_ocaml _; _}
    | OCaml _ -> Some "ocaml", pp_code
    | Cram _ -> Some "bash", pp_code
    | Include {file_kind = Fk_other {header}; _}
    | Raw {header} -> Option.map header_to_string header, pp_code
  in
  let lang = match lang with
    | None   -> "clike"
    | Some l -> l
  in
  Fmt.pf ppf "```%s\n%t\n```"
    lang pp_code

let pp_text_html ppf l =
  List.iter (Fmt.pf ppf "%s\n") (List.rev l)

let pp_text_md ppf l =
  List.iter (Fmt.pf ppf "%s\n") (List.rev l)

open Astring

let pp_text_latex ppf l =
  let l = List.rev l in
  let t = String.concat ~sep:"\n" l in
  let rec f t acc =
    match String.find_sub ~rev:true ~sub:"]{.idx}" t with
    | None -> t::acc
    | Some i ->
      begin
        match String.find ~rev:true ~start:i (Char.equal '[') t with
        | None -> t::acc
        | Some j ->
          let escape_latex acc c =
            match c with
              | '&' | '%' | '$' | '#' | '_' | '{' | '}' | '~' | '^' |  '\\' ->
                ("\\" ^ (String.of_char c)) :: acc
              | _ -> String.of_char c :: acc
          in
          let b = String.sub ~stop:j t |> String.Sub.to_string in
          let m = String.sub ~start:(j + 1) ~stop:i t
            |> String.Sub.to_string
            |> String.fold_left escape_latex []
            |> List.rev
            |> String.concat
            |> Base.String.map ~f:(function '/' -> '!' | x -> x)
            |> Fmt.str "\\index{%s}"
          in
          let e = String.sub ~start:(i + 7) t |> String.Sub.to_string in
          f b (m::e::acc)
      end
  in
  let t = f t [] in
  List.iter (Fmt.pf ppf "%s") t;
  Fmt.pf ppf "\n"

type outputs =
    Html
  | Markdown
  | Latex

let get_output_infos = function
  | Html -> pp_block_html, pp_text_html, "-t html"
  | Latex -> pp_block_latex, pp_text_latex,  "-t latex --listings"
  | Markdown -> pp_block_md, pp_text_latex, "-t markdown"

let run (`File file) (`Output output) output_type =
  let (pp_block, pp_text, out_args) = get_output_infos output_type in
  match Mdx.parse_file Normal file with
  | Error (`Msg msg) ->
    Printf.eprintf "%s\n" msg;
    1
  | Ok [] -> 1
  | Ok t  ->
    let tmp = Filename.temp_file "ocaml-mdx" "pandoc" in
    let oc = open_out tmp in
    let ppf = Format.formatter_of_out_channel oc in
    let boost_section (i,t) =
      match output_type with
      | Markdown -> i+1, t (* for top-level-division=part in pandoc *)
      | _ -> i, t in
    let f acc t =
      match t with
        | Mdx.Section s ->
          Fmt.pf ppf "%a%a" pp_text acc pp_section (boost_section s);
          []
        | Text t        -> t::acc
        | Block b ->
          Fmt.pf ppf "%a%a" pp_text acc pp_block b;
          []
    in
    Fmt.pf ppf "%a%!" pp_text (List.fold_left f [] t);
    close_out oc;
    let output = match output with None -> "-" | Some o -> o in
    Fmt.pr "Generating %s...\n%!" output;
    Fmt.kstr
      Sys.command
      "pandoc \
      \  --section-divs \
      \  -f markdown-ascii_identifiers \
      \  --no-highlight\
      \  %s %s -o %s"
      out_args tmp output

open Cmdliner

let file =
  let doc = "The file to use." in
  Term.(app (const (fun x -> `File x)))
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FILE")

let output =
  let doc = "Write output to $(b,FILE) instead of stdout.  If $(b,FILE) is -, \
             output will go to stdout," in
  Term.(app (const (fun x -> `Output x)))
    Arg.(value & opt (some string) None & info ["o";"output"] ~doc ~docv:"FILE")

let out_type =
  let doc = "Output type: html, md or latex. Default is html" in
  Arg.(value & opt (enum ["html", Html; "latex", Latex; "md", Markdown]) Html & info ["t";"type"] ~doc ~docv:"TYPE")

let main =
  let doc = "Pre-process markdown files to produce OCaml code." in
  let exits = Cmd.Exit.defaults in
  let term = Term.(const run $ file $ output $ out_type) in
  Cmd.(v (info "output"~doc ~exits) term)

let () = exit @@ Cmd.eval' ~catch:false main

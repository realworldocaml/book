(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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
 *)

let src = Logs.Src.create "cram.pp"
module Log = (val Logs.src_log src : Logs.LOG)

let pp_section ppf (n, title) =
  let pp_n ppf () = Fmt.string ppf (String.make n '#') in
  Fmt.pf ppf "%a %s\n" pp_n () title

let pp_list pp = Fmt.(list ~sep:(unit "") pp)

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

let pp_line ppf l = Fmt.pf ppf "%a\n" pp_html l

let pp_toplevel ppf (t:Mdx.Toplevel.t) =
  let cmds = match t.command with [c] -> [c ^ ";;"] | l -> l @ [";;"] in
  Fmt.pf ppf "%a%a" (pp_list pp_line) cmds (pp_list pp_output) t.output

let pp_contents (t:Mdx.Block.t) ppf =
  Fmt.(list ~sep:(unit "\n") pp_html) ppf t.contents

let pp_cram ppf (t:Mdx.Cram.t) =
  let pp_exit ppf = match t.exit_code with
    | 0 -> ()
    | i -> Fmt.pf ppf "[%d]" i
  in
  Fmt.pf ppf "%a%a%t"
    (pp_list pp_line) t.command
    (pp_list pp_output) t.output pp_exit

let pp_block ppf (b:Mdx.Block.t) =
  let lang, pp_code, attrs = match b.value with
    | Toplevel t -> Some "ocaml", (fun ppf -> pp_list pp_toplevel ppf t), [
        ("class"             , "command-line");
        ("data-prompt"       , "#");
        ("data-filter-output", ">");
      ]
    | OCaml  -> Some "ocaml", pp_contents b, []
    | Cram t -> Some "bash" , (fun ppf -> pp_list pp_cram ppf t.tests), [
        ("class"             , "command-line");
        ("data-user"         , "fun");
        ("data-host"         , "lama");
        ("data-filter-output", ">");
      ]
    | Raw     -> b.header, pp_contents b, []
    | Error s -> Some "error", (fun ppf -> pp_list Fmt.string ppf s), []
  in
  let pp_attr ppf (k, v) = Fmt.pf ppf "%s=%S" k v in
  let pp_lang ppf () = match lang with
    | None   -> Fmt.pf ppf " class=\"language-clike\""
    | Some l -> Fmt.pf ppf " class=\"language-%s\"" l
  in
  let pp_attrs ppf () = match attrs with
    | [] -> ()
    | _  -> Fmt.pf ppf " %a" Fmt.(list ~sep:(unit " ") pp_attr) attrs
  in
  Fmt.pf ppf "<div class=\"highlight\"><pre%a><code%a>%t</code></pre></div>"
    pp_attrs () pp_lang () pp_code

let run () file output =
  let t = Mdx.parse_file Normal file in
  match t with
  | [] -> 1
  | _  ->
    let tmp = Filename.temp_file "ocaml-mdx" "pandoc" in
    let oc = open_out tmp in
    let ppf = Format.formatter_of_out_channel oc in
    List.iter (function
        | Mdx.Section s -> Fmt.pf ppf "%a" pp_section s
        | Text t        -> Fmt.pf ppf "%s\n" t
        | Block b ->
          let b = Mdx.Block.eval b in
          Log.debug (fun l -> l "output: %a" Mdx.Block.dump b);
          Fmt.pf ppf "%a\n" pp_block b
      ) t;
    Fmt.pf ppf "%!";
    close_out oc;
    let output = match output with None -> "-" | Some o -> o in
    Fmt.pr "Generating %s...\n%!" output;
    Fmt.kstrf
      Sys.command
      "pandoc \
      \  --section-divs \
      \  -f markdown-ascii_identifiers \
      \  --no-highlight\
      \  -t html5 %s -o %s"
      tmp output

open Cmdliner

let output =
  let doc = "Write output to $(b,FILE) instead of stdout.  If $(b,FILE) is -, \
             output will go to stdout," in
  Arg.(value & opt (some string) None & info ["o";"output"] ~doc ~docv:"FILE")

let cmd =
  let doc = "Pre-process markdown files to produce OCaml code." in
  let exits = Term.default_exits in
  Term.(pure run $ Cli.setup $ Cli.file $ output),
  Term.info "output" ~doc ~exits

(** Generate dune files for code examples within the tree *)
open Printf
open Sexplib.Conv

let (/) = Filename.concat

(** Directory and file traversal functions *)

(** [find_dirs_containing ~exts base] will return all the sub-directories
    that contain files that match any of extension [ext], starting from
    the [base] directory. *)
let find_dirs_containing ?(ignore_dirs=[]) ~exts base =
  let rec fn base =
    Sys.readdir base |>
    Array.map (Filename.concat base) |>
    Array.map (fun d ->
      if Sys.is_directory d && not (List.mem (Filename.basename d) ignore_dirs) then
        fn d else [d]) |>
    Array.to_list |>
    List.flatten |>
    List.filter (fun f -> List.exists (Filename.check_suffix f) exts) in
  fn base |>
  List.map Filename.dirname |>
  List.sort_uniq String.compare

(** [files_with ~exts base] will return all the files matching the
    extension [exts] in directory [base]. *)
let files_with ~exts base =
  Sys.readdir base |>
  Array.to_list |>
  List.filter (fun f -> List.exists (Filename.check_suffix f) exts) |>
  List.sort_uniq String.compare

let emit_file file s =
  eprintf "Processing %s\n%!" file;
  let fout = open_out file in
  output_string fout s;
  close_out fout

let book_extensions =
  [ ".ml"; ".mli"; ".mly"; ".mll";
    ".syntax"; ".scm"; ".rawscript"; ".java"; ".cpp";
    ".mlt"; ".sh"; ".errsh"; ".rawsh"; "dune";
    ".json"; ".atd"; ".rawsh"; ".c"; ".h"; ".cmd"; ".S" ]

let static_extensions =
  [ ".js"; ".jpg"; ".css"; ".png" ]

(** Process the book chapters *)

type part = {
  title   : string;
  chapters: string list;
} [@@deriving sexp]

type toc = [ `part of part | `chapter of string] list [@@deriving sexp]

let toc_file = "toc.scm"

let toc_files toc =
  let part acc = function
    | `part p    -> List.rev_append p.chapters acc
    | `chapter f -> f :: acc
  in
  let toc = List.fold_left part [] toc in
  List.sort String.compare toc

let dune_for_chapter file =
  let file = (Filename.remove_extension file) ^ ".html" in
  sprintf {|(alias (name site) (deps %s))

(rule
  (targets %s)
  (deps    (:x ../book/%s)
           (alias ../book/html)
           ../bin/bin/app.exe
           ../book/%s)
  (action  (run rwo-build build chapter -o . -repo-root .. %%{x})))|}
    file file file toc_file

let read_toc base_dir =
  let f = base_dir / toc_file in
  let s = Sexplib.Sexp.load_sexps f in
  toc_of_sexp (Sexplib.Sexp.List s)

let is_chapter (toc:toc) f =
  let mem = function
    | `chapter c -> String.equal c f
    | `part p    -> List.mem f p.chapters
  in
  List.exists mem toc

let frontpage_chapter ?(deps=[]) name =
  sprintf {|(alias (name site) (deps %s.html))
  (rule
    (targets %s.html)
    (deps    (alias ../book/html) ../book/%s.html ../bin/bin/app.exe %s)
    (action  (run rwo-build build %s -o . -repo-root ..)))|}
    name name name
    (String.concat " " deps)
    name

let find_static_files () =
  find_dirs_containing ~exts:static_extensions "static" |>
  List.map (fun d ->
      files_with ~exts:static_extensions d |> List.map (Filename.concat d)) |>
  List.flatten |>
  List.map (fun f ->
    String.split_on_char '/' f |>
    List.tl |>
    String.concat "/") |>
  String.concat "\n        " |>
  sprintf {|(alias
  (name site)
  (deps %s))|}

let process_chapters ~toc book_dir output_dir =
  let html =
    files_with ~exts:[".html"] book_dir |>
    List.sort String.compare |>
    List.map (function
        | "install.html" -> frontpage_chapter "install"
        | "faqs.html" -> frontpage_chapter "faqs"
        | "toc.html" -> frontpage_chapter ~deps:["../book/"^toc_file] "toc"
        | "index.html" -> frontpage_chapter "index"
        | file -> eprintf "Warning: orphan html file %s in repo\n" file; ""
      ) |>
    List.filter ((<>)"")
  in
  let chapters = List.map dune_for_chapter (toc_files toc) in
  html @ chapters |>
  String.concat "\n\n" |> fun s ->
  find_static_files () ^ s  ^ "\n" |>
  emit_file (output_dir / "dune")

let process_md ~toc book_dir =
  let toc = toc_files toc in
  let html_alias =
    let file f = f ^ ".html" in
    let toc = List.map file toc in
    let toc = String.concat "\n        " toc in
    sprintf "(alias\n  (name html)\n  (deps %s))" toc
  in
  let main_dune () =
    List.map (fun chapter ->
        let html = chapter ^ ".html" in
        sprintf {|(rule
  (targets %s)
  (deps    %s)
  (action  (run mdx output %%{deps} -o %%{targets})))|}
          html (chapter / "README.md")
      ) toc |>
    (fun x -> html_alias :: x) |>
    String.concat "\n\n" |>
    (fun x -> x ^ "\n") |>
    emit_file (book_dir / "dune")
  in
  main_dune ()

let _ =
  let toc = read_toc "book" in
  process_md ~toc "book";
  process_chapters ~toc "book" "static";

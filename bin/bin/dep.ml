(** Generate dune files for code examples within the tree *)
open Printf
open Sexplib.Conv

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

(** Find the dependencies within an HTML file *)
let deps_of_chapter file =
  let open Soup in
  read_file file |>
  parse |> fun s ->
  s $$ "link[rel][href]" |>
  fold (fun a n -> R.attribute "href" n :: a) [] |>
  List.sort_uniq String.compare

let toc_file = "toc.scm"

let dune_for_chapter base_dir file =
  let examples_dir = "../examples" in
  let pad = "\n           " in
  let deps =
    deps_of_chapter (Filename.concat base_dir file) |>
    List.map (fun f -> sprintf "%s/%s" examples_dir f) |>
    List.map (fun s -> s) |>
    String.concat pad
    |> function
    | "" -> ""
    | s  -> pad ^ s
  in
  let file = (Filename.remove_extension file) ^ ".html" in
  sprintf {|(alias (name site) (deps %s))

(rule
  (targets %s)
  (deps    (:x ../book/%s)
           (alias ../book/html)
           ../bin/bin/app.exe
           ../book/%s%s)
  (action  (run rwo-build build chapter -o . -code ../examples -repo-root .. %%{x})))|}
    file file file toc_file deps

type part = {
  title   : string;
  chapters: string list;
} [@@deriving sexp]

type toc = [ `part of part | `chapter of string] list [@@deriving sexp]

let read_toc base_dir =
  let f = Filename.concat base_dir toc_file in
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
  files_with ~exts:[".html";".md"] book_dir |>
  List.sort String.compare |>
  List.map (function
    | file when is_chapter toc file -> dune_for_chapter book_dir file
    | "install.html" -> frontpage_chapter "install"
    | "faqs.html" -> frontpage_chapter "faqs"
    | "toc.html" -> frontpage_chapter ~deps:["../book/"^toc_file] "toc"
    | "index.html" -> frontpage_chapter "index"
    | file -> eprintf "Warning: orphan html file %s in repo\n" file; ""
    ) |>
  List.filter ((<>)"") |>
  String.concat "\n\n" |> fun s ->
  find_static_files () ^ s  ^ "\n" |>
  emit_file (Filename.concat output_dir "dune")

(** Handle examples *)

(* build a valid dune alias name *)
let to_alias s = String.map (function
    | '/' -> '-'
    | c   -> c
  ) s

let mlt_rule ~dir base =
  let alias = to_alias (Filename.concat dir base) in
  let alias name cmd =
    sprintf {|(alias
 (name    %s-%s)
 (deps   (source_tree %s))
 (action
   (chdir %s
    (progn
     (setenv OCAMLRUNPARAM "" (run %s %s))
     (diff? %s %s.corrected)))))

(alias
  (name %s)
  (deps (alias %s-%s)))|}
      name alias dir dir cmd base base base name name alias
  in
  sprintf "%s\n\n%s\n"
    (alias "runtest"     "ocaml-topexpect -short-paths -verbose")
    (alias "runtest-all" "ocaml-topexpect -non-deterministic -short-paths -verbose")

let sh_rule ~dir base =
  let alias = to_alias (Filename.concat dir base) in
  let alias name cmd =
    sprintf {|(alias
 (name     %s-%s)
 (deps     (source_tree %s))
 (action
   (chdir %s
    (progn
     (run %s %s)
     (diff? %s %s.corrected)))))

(alias
  (name %s)
  (deps (alias %s-%s)))|}
      name alias dir dir cmd base base base name name alias
  in
  sprintf "%s\n\n%s\n"
    (alias "runtest"     "cram")
    (alias "runtest-all" "cram --non-deterministic")

let process_example ~root dir =
  let rdir =
    if root = dir then "."
    else
      let rlen = String.length root + 1 in
      String.sub dir rlen (String.length dir - rlen)
  in
  files_with ~exts:book_extensions dir |>
  List.map (function
      | f when Filename.extension f = ".mlt"   -> mlt_rule ~dir:rdir f
      | f when Filename.extension f = ".sh"    -> sh_rule ~dir:rdir f
      | f when Filename.extension f = ".errsh" -> sh_rule ~dir:rdir f
      | f -> printf "skipping %s/%s\n%!" dir f; ""
    ) |>
  List.filter ((<>) "")

let process_examples dir =
  let dirs =
    find_dirs_containing ~ignore_dirs:["_build"] ~exts:book_extensions dir
  in
  Filename.concat dir "dune" |> fun dune ->
  List.map (process_example ~root:dir) dirs |>
  List.flatten |>
  List.sort_uniq String.compare |>
  String.concat "\n" |>
  fun x ->  emit_file dune ("(ignored_subdirs (code))\n\n" ^ x)

let process_md ~toc book_dir =
  let html_alias =
    let file f = (Filename.chop_extension f) ^ ".html" in
    let part acc = function
      | `part p    -> List.rev_append (List.rev_map file p.chapters) acc
      | `chapter f -> file f :: acc
    in
    let toc = List.fold_left part [] toc in
    let toc = List.sort String.compare toc in
    let toc = String.concat "\n         " toc in
    sprintf "(alias\n  (name html)\n  (deps %s))" toc
  in
  files_with ~exts:[".md"] book_dir |>
  List.sort String.compare |>
  List.map (fun file ->
      if not (is_chapter toc file) then ""
      else
        let html = (Filename.remove_extension file) ^ ".html" in
        sprintf {|(rule
 (targets %s)
 (deps    %s)
 (action  (run
    mdx output %%{deps} -o %%{targets})))|}
          html file
    ) |>
  List.filter ((<>) "") |>
  (fun x -> html_alias :: x) |>
  String.concat "\n\n" |>
  (fun x -> x ^ "\n") |>
  emit_file (Filename.concat book_dir "dune")

let _ =
  let toc = read_toc "book" in
  process_md ~toc "book";
  process_examples "examples";
  process_chapters ~toc "book" "static";

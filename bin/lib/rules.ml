open Core
open Async

let (/) = Filename.concat

let print_web ~repo_root ~include_wip =
  let out = Lazy.force Writer.stdout in
  let book_folder = repo_root / "book" in
  let toc_file = book_folder / "toc.scm" in
  let html_alias = book_folder / "html" in
  let alias = if include_wip then "site-wip" else "site" in
  Toc.Repr.get_chapters ~repo_root ~include_wip () >>| fun chapters ->
  chapters
  |> List.sort ~compare:(fun c c' -> String.compare c.name c'.name)
  |> List.iter
    ~f:(fun chapter ->
        let html_file = book_folder / (chapter.name ^ ".html") in
        let target = chapter.name ^ ".html" in
        let include_wip = if include_wip then "-include-wip " else "" in
        Writer.writef out
          {|
(rule
 (alias %s)
 (target %s)
 (deps (alias %s) %s)
 (action (run rwo-build build chapter -o . -repo-root %s %s%%{dep:%s})))
|}
          alias
          target
          html_alias
          toc_file
          repo_root
          include_wip
          html_file)

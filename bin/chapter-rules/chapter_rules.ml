module List = ListLabels

let failf fmt =
  Format.ksprintf (fun msg -> Format.eprintf "%s\n" msg; exit 1) fmt

let print_rules dir_name =
  Printf.printf
    {|
(rule
 (alias html)
 (target %s.html)
 (action (run rwo-convert_md  %%{dep:%s/README.md} -t html -o %%{target})))

(rule
 (alias latex)
 (target %s.tex)
 (action (run rwo-convert_md  %%{dep:%s/README.md} -t latex -o %%{target})))
|}
    dir_name
    dir_name
    dir_name
    dir_name

let run book_dir =
  let is_dir path = Filename.concat book_dir path |> Sys.is_directory in
  let sub_dirs =
    Sys.readdir book_dir
    |> Array.to_list
    |> List.sort ~cmp:String.compare
    |> List.filter ~f:is_dir
  in
  List.iter sub_dirs ~f:print_rules

let () =
  match Sys.argv.(1) with
  | book_dir -> run book_dir
  | exception Invalid_argument _ ->
    failf "Requires path to the book directory as first argument"

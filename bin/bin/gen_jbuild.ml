(** Generate jbuild files for code examples within the tree *)
open Printf

(** [find_dirs_containing ~ext base] will return all the sub-directories
    that contain files that match extension [ext], starting from the [base]
    directory. *)
let rec find_dirs_containing ~ext base =
  let rec fn base =
    Sys.readdir base |>
    Array.map (Filename.concat base) |>
    Array.map (fun d -> if Sys.is_directory d then fn d else [d]) |>
    Array.to_list |>
    List.flatten |>
    List.filter (fun f -> Filename.check_suffix f ext) in
  fn base |>
  List.map Filename.dirname |>
  List.sort_uniq String.compare

(** [files_with ~ext base] will return all the files matching the
    extension [ext] in directory [base]. *)
let files_with ~ext base =
  Sys.readdir base |>
  Array.to_list |>
  List.filter (fun f -> Filename.check_suffix f ext) 

let process_topscripts dir =
  eprintf "Processing %s\n%!" dir;
  let jbuild = Filename.concat dir "jbuild" in
  let ts = files_with ~ext:".topscript" dir in
  let rule f = sprintf "
(alias ((name code) (deps (%s.stamp))))
(rule
 ((targets (%s.sexp))
  (deps    (%s))
  (action  (with-stdout-to ${@} (run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))
(rule
 ((targets (%s.stamp))
  (deps    (%s))
  (action  (progn 
    (run ocaml-topexpect -short-paths -verbose ${<})
    (write-file ${@} \"\")
    (promote-if (%s.corrected as %s))
  ))))" f f f f f f f in
  let fout = open_out jbuild in
  fprintf fout "(jbuild_version 1)\n%s\n" (List.map rule ts |> String.concat "\n");
  close_out fout

let _ =
  find_dirs_containing ~ext:".topscript" Sys.argv.(1) |>
  List.iter process_topscripts


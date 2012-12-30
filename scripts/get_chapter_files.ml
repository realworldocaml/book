(* This script reads chapters.scm and outputs a list of filenames,
   with a prefix before each one. *)
open Core.Std
open Chapter

let get_files parts prefix public =
  let parts = Sexp.load_sexps_conv_exn parts chapter_of_sexp in
  List.iter parts ~f:(fun c -> 
    match public, c.public with
    |true, true 
    |false, _ -> print_endline (prefix^c.file^".md")
    |true, false -> ())

open Cmdliner
let _ = 
  let parts = Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"CHAPTERS"
     ~doc:"Sexp list of chapters and the parts that they map onto. See $(b,type chapter) in $(i,add_parts.ml) for the format of this file.") in
  let prefix = Arg.(required & pos 1 (some string) None & info [] ~docv:"PREFIX"
    ~doc:"Prefix to prepend before each chapter filename before outputting them.") in
  let public = Arg.(value & flag & info ["public"] ~doc:"Filter the chapter list to only output the ones marked as $(i,public) in the $(i,CHAPTERS) file.") in
  let info = Term.info "get_chapter_files" ~version:"1.0.0" ~doc:"get source filenames from a chapter list" in
  let cmd_t = Term.(pure get_files $ parts $ prefix $ public) in
  match Term.eval (cmd_t, info) with `Ok x -> x |_ -> exit 1


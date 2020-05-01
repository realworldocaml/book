(* This script finds the names of the modules in MenhirLib by reading the file
   menhirLib.mlpack. It then finds the source files for these modules in the
   parent directory (lib/), and concatenates them to create menhirLib.{ml,mli}
   in the current directory (lib/pack). *)

(* ------------------------------------------------------------------------- *)

(* [up fn] is [../fn]. *)

let up fn =
  Filename.concat Filename.parent_dir_name fn

(* ------------------------------------------------------------------------- *)

(* [cat_file oc fn] prints the content of the file [fn] on the channel [oc]. *)

let cat_file oc fn =
  let ic = open_in fn in
  let rec loop () =
    match input_line ic with
    | s -> output_string oc s; output_char oc '\n'; loop ()
    | exception End_of_file -> ()
  in
  loop ();
  close_in ic

(* ------------------------------------------------------------------------- *)

(* The names of the modules in MenhirLib are obtained by reading the
   non-comment lines in the file menhirLib.mlpack. *)

let menhirLib_modules : string list =
  let ic = open_in "menhirLib.mlpack" in
  let rec loop accu =
    match input_line ic with
    | exception End_of_file -> List.rev accu
    | s ->
        let s = String.trim s in
        let accu =
          if s <> "" && s.[0] <> '#' then
            s :: accu
          else
            accu
        in
        loop accu
  in
  let r = loop [] in
  close_in ic;
  r

(* ------------------------------------------------------------------------- *)

(* The source file menhirLib.ml is created by concatenating all of the source
   files that make up MenhirLib. This file is not needed to compile Menhir or
   MenhirLib. It is installed at the same time as MenhirLib and is copied by
   Menhir when the user requests a self-contained parser (one that is not
   dependent on MenhirLib). *)

let () =
  print_endline "Creating menhirLib.ml...";
  let oc = open_out "menhirLib.ml" in
  List.iter (fun m ->
    Printf.fprintf oc "module %s = struct\n" m;
    cat_file oc (up (m ^ ".ml"));
    Printf.fprintf oc "end\n"
  ) menhirLib_modules;
  close_out oc

(* The source file menhirLib.mli is created in the same way. If a module
   does not have an .mli file, then we assume that its .ml file contains
   type (and module type) definitions only, so we copy it instead of the
   (non-existent) .mli file. *)

let () =
  print_endline "Creating menhirLib.mli...";
  let oc = open_out "menhirLib.mli" in
  List.iter (fun m ->
      Printf.fprintf oc "module %s : sig\n" m;
      if Sys.file_exists (up (m ^ ".mli")) then
        cat_file oc (up (m ^ ".mli"))
      else
        cat_file oc (up (m ^ ".ml"));
      Printf.fprintf oc "end\n"
    ) menhirLib_modules;
  close_out oc

(* Dump the paragraph fragments and into a sexp *)
(* Usage: cmd output-file [input file1] [input file2...] *)
open Core.Std
open Xml_tree
open Para_frag

let odir, files =
  match Array.to_list Sys.argv with
  |_::hd::tl -> hd, tl
  |_ -> failwith "Usage: output-dir <input files>"

let ts = ref []

let filter dtd file i =
  let rec aux = function
    | Element ((("","p"),attr),c) -> begin
      match List.Assoc.find attr ("","id") with
      | None -> ()
      | Some pid -> begin
        let buf = Buffer.create 1 in
        let xo = Xmlm.make_output ~decl:false (`Buffer buf) in
        let tag = mk_tag "blockquote" c in
        out_tree xo (dtd,tag);
        let t = { file=Filename.basename file; html=Buffer.contents buf} in
        match List.Assoc.find !ts pid with
        | Some _ -> failwith (pid ^ " clash!")
        | None -> ts := List.Assoc.add !ts pid t
      end
    end
    | Element (_p,c) -> List.iter ~f:aux c
    | Data _ -> ()
  in
  aux i

let _ =
  Printf.printf "output file: %s\n" odir;
  Printf.printf "to read: %s\n%!" (String.concat ~sep:", " files);
  List.iter files ~f:(fun file ->
    let buf = In_channel.read_all file in
    try
      let i = Xmlm.make_input ~entity:Xhtml.entity (`String (0,buf)) in
      let (dtd,it) = in_tree i in
      filter dtd file it
    with Xmlm.Error (_p,e) -> begin
      Printf.eprintf "FATAL: %s\n" (Xmlm.error_message e);
      exit 1
    end
  );
  Sexp.save_hum odir (sexp_of_ts !ts);

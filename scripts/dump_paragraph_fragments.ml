(* Dump the paragraph fragments and into a sexp *)
(* Usage: cmd output-file [input file1] [input file2...] *)
open Core.Std
open Para_frag

let odir, files =
  match Array.to_list Sys.argv with
  |_::hd::tl -> hd, tl
  |_ -> failwith "Usage: output-dir <input files>"

let ts = ref []

let dump_frags file attr c =
  match List.Assoc.find attr ("","id") with
  | None -> ()
  | Some pid -> begin
    let html = Cow.Html.to_string <:html<<blockquote>$c$</blockquote>&>> in
    let t = { file=Filename.basename file; html } in
    match List.Assoc.find !ts pid with
    | Some _ -> failwith (pid ^ " clash!")
    | None -> ts := List.Assoc.add !ts pid t
   end

let _ =
  Printf.printf "output file: %s\n" odir;
  Printf.printf "to read: %s\n%!" (String.concat ~sep:", " files);
  List.iter files ~f:(fun file ->
    let buf = In_channel.read_all file in
    try
      Xml_tree.read_document (open_in file) 
      |> fun (_dtd,doc) ->
      Xml_tree.iter ~tag:"p" ~f:(dump_frags file) doc
    with Xmlm.Error (_p,e) -> begin
      Printf.eprintf "FATAL: %s %s\n" file (Xmlm.error_message e);
      print_endline buf;
      exit 1
    end
  );
  Sexp.save_hum odir (sexp_of_ts !ts);

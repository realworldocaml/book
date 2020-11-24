open Printf

let () =
  let file = ref None in
  let dic = ref [] in
  let dic_file = ref (Bi_dump.default_dict_path ()) in
  let use_global_dictionary = ref true in
  let options = [
    "-d", Arg.String (fun s -> dic := Bi_dump.load_lines !dic s),
    "file
          File containing words to add to the dictionary, one per line";

    "-h", Arg.String (fun s -> dic_file := Some s),
    "file
          Location of the dictionary used for unhashing.
          Default: $HOME/.bdump-dict on Unix,
                   $HOMEPATH\\_bdump-dict on Windows";

    "-w", Arg.String (fun s -> dic := List.rev_append (Bi_dump.split s) !dic),
    "word1,word2,...
          Comma-separated list of words to add to the dictionary";

    "-x", Arg.Clear use_global_dictionary,
    sprintf "
          Do not load nor update the dictionary used for name
          unhashing.";
  ]
  in
  let msg = sprintf "Usage: %s [file] [options]" Sys.argv.(0) in
  let error () =
    Arg.usage options msg in
  let set_file s =
    match !file with
	None -> file := Some s
      | Some _ -> error ()
  in
  Arg.parse options set_file msg;

  if !use_global_dictionary then (
    let must_save = !dic <> [] in
    dic := Bi_dump.load_dictionary !dic_file !dic;
    if must_save then
      Bi_dump.save_dictionary !dic_file !dic
  );

  let unhash = Bi_io.make_unhash !dic in
  let ic =
    match !file with
	None -> stdin
      | Some s -> open_in_bin s
  in
  let inbuf = Bi_inbuf.from_string (Bi_dump.load ic) in
  let value_count = ref 0 in
  Printexc.record_backtrace true;
  (try
    while true do
      (try ignore (Bi_inbuf.peek inbuf)
       with Bi_inbuf.End_of_input -> raise Exit);
      Bi_io.print_view_of_tree (Bi_io.read_tree ~unhash inbuf);
      print_newline ();
      incr value_count;
    done;
   with
       Exit -> ()
     | e ->
         Printf.eprintf "Broken input after reading %i value%s: \
                         exception %s\n"
           !value_count (if !value_count > 1 then "s" else "")
           (Printexc.to_string e);
         Printexc.print_backtrace stderr;
         flush stderr
  );

  close_in ic

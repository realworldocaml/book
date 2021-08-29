open StdLabels

let read_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let file_contents = really_input_string ic len in
  close_in ic;
  file_contents

let run_expect_test file ~f =
  let file_contents = read_file file in
  let lexbuf = Lexing.from_string file_contents in
  lexbuf.lex_curr_p <-
    { pos_fname = file
    ; pos_cnum  = 0
    ; pos_lnum  = 1
    ; pos_bol   = 0
    };

  let expected = f file_contents lexbuf in

  let corrected_file = file ^ ".corrected" in
  if file_contents <> expected then begin
    let oc = open_out_bin corrected_file in
    output_string oc expected;
    close_out oc;
  end else begin
    if Sys.file_exists corrected_file then Sys.remove corrected_file;
    exit 0
  end

let print_loc _ _ ppf (loc : Location.t) =
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  Format.fprintf ppf "Line _";
  if startchar >= 0 then
    Format.fprintf ppf ", characters %d-%d" startchar endchar;
  Format.fprintf ppf ":@."

let report_printer () =
  let printer = Location.default_report_printer () in
  { printer with Location. pp_main_loc = print_loc; pp_submsg_loc = print_loc; }

let setup_printers ppf =
  Location.formatter_for_warnings := ppf;
  Location.warning_reporter := Location.default_warning_reporter;
  Location.report_printer   := report_printer;
  Location.alert_reporter   := Location.default_alert_reporter

let apply_rewriters : (Parsetree.toplevel_phrase -> Parsetree.toplevel_phrase) = function
  | Ptop_dir _ as x -> x
  | Ptop_def s ->
    let s = Ppxlib.Selected_ast.of_ocaml Structure s in
    let s' = Ppxlib.Driver.map_structure s in
    Ptop_def (Ppxlib.Selected_ast.to_ocaml Structure s')

let main () =
  run_expect_test Sys.argv.(1) ~f:(fun file_contents lexbuf ->
    let chunks = Expect_lexer.split_file ~file_contents lexbuf in

    let buf = Buffer.create (String.length file_contents + 1024) in
    let ppf = Format.formatter_of_buffer buf in
    setup_printers ppf;
    Topfind.log := ignore;

    Warnings.parse_options false "@a-4-29-40-41-42-44-45-48-58";
    Clflags.real_paths := false;
    Toploop.initialize_toplevel_env ();

    (* Findlib stuff *)
    let preds = ["toploop"] in
    let preds =
      match Sys.backend_type with
      | Native -> "native" :: preds
      | Bytecode -> "byte" :: preds
      | Other _ -> preds
    in
    Topfind.add_predicates preds;
    (* This just adds the include directories since the [ppx] library
       is statically linked in *)
    Topfind.load_deeply ["ppxlib"];

    List.iter chunks ~f:(fun (pos, s) ->
      Format.fprintf ppf "%s[%%%%expect{|@." s;
      let lexbuf = Lexing.from_string s in
      lexbuf.lex_curr_p <- { pos with pos_lnum = 1; };
      let phrases = !Toploop.parse_use_file lexbuf in
      List.iter phrases ~f:(function
        | Parsetree.Ptop_def [] -> ()
        | phr ->
          try
            let phr = apply_rewriters phr in
            if !Clflags.dump_source then
              Format.fprintf ppf "%a@?" Pprintast.top_phrase phr;
            ignore (Toploop.execute_phrase true ppf phr : bool)
          with exn ->
            Location.report_exception ppf exn
      );
      Format.fprintf ppf "@?|}]@.");
    Buffer.contents buf)

let () =
  try
    main ()
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1

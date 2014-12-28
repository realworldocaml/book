(* Syntax hightlight code and eval ocaml toplevel phrases.
 * Based on http://github.com/ocaml/ocaml.org 
 * Modified by Anil Madhavapeddy for Real World OCaml and to use Core *)
open Core.Std
open Rwo

(** Run these phrases silently before any code *)
let initial_phrases = [
  "#use \"topfind\"";
  "#camlp4o";
  "#require \"core\"";
  "#require \"core.syntax\"";
  "#require \"core.top\"";
  "open Core.Std" ]

(** Initialise toploop and turn on short-paths *)
let reset_toplevel () : unit =
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "//toplevel//";
  Topdirs.dir_directory (Sys.getenv_exn "OCAML_TOPLEVEL_PATH");
  Clflags.real_paths := false

(** Read data from given file descriptor once it is ready. Return when
    no new data shows up for a while, 0.001 seconds. *)
let string_of_fd (fd:Unix.File_descr.t) : string =
  let is_ready_for_read fd : bool =
    let {Unix.Select_fds.read; _} =
      Unix.select ~read:[fd] ~write:[] ~except:[] ~timeout:(`After 0.001) ()
    in
    read <> []
  in
  let buf = Buffer.create 1024 in
  let s = String.create 256 in
  while is_ready_for_read fd do
    let r = Unix.read fd ~buf:s ~pos:0 ~len:256 in
    Buffer.add_substring buf s 0 r
  done;
  Buffer.contents buf

let flush_std_out_err () =
  Format.pp_print_flush Format.std_formatter ();
  flush stdout;
  Format.pp_print_flush Format.err_formatter ();
  flush stderr

(** Evaluate double-semicolon terminated phrase through the
    toploop. State of the toplevel environment is relevant. You may
    want to call [reset_toplevel]. *)
let toploop_eval (input:string) : Code.phrase =
  if String.strip input = ";;" then
    {Code.input; output = ""; stdout = ""; stderr = ""}
  else (
    let init_stdout = Unix.dup Unix.stdout in
    let init_stderr = Unix.dup Unix.stderr in
    flush_std_out_err ();
    let (out_in, out_out) = Unix.pipe() in
    Unix.dup2 out_out Unix.stdout; (* Unix.stdout → out_out *)
    let (err_in, err_out) = Unix.pipe() in
    Unix.dup2 err_out Unix.stderr; (* Unix.stderr → err_out *)
    let get_stdout_stderr_and_restore () =
      flush_std_out_err ();
      let out = string_of_fd out_in in
      Unix.close out_in;
      Unix.close out_out;
      Unix.dup2 init_stdout Unix.stdout; (* restore initial stdout *)
      let err = string_of_fd err_in in
      Unix.close err_in;
      Unix.close err_out;
      Unix.dup2 init_stderr Unix.stderr; (* restore initial stderr *)
      (out, err) in
    try
      let lexbuf = Lexing.from_string input in
      let dummypos = { Lexing.pos_fname = "//toplevel//"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1; } in
      lexbuf.Lexing.lex_start_p <- dummypos;
      lexbuf.Lexing.lex_curr_p <- dummypos;
      let phrase = !Toploop.parse_toplevel_phrase lexbuf in
      ignore(Toploop.execute_phrase true Format.str_formatter phrase);
      let output = Format.flush_str_formatter () in
      let stdout,stderr = get_stdout_stderr_and_restore () in
      {Code.input; output; stdout; stderr}
    with e -> (
      let stdout,stderr = get_stdout_stderr_and_restore () in
      let backtrace_enabled = Printexc.backtrace_status () in
      if not backtrace_enabled then Printexc.record_backtrace true;
      let output =
        try
          Errors.report_error Format.str_formatter e;
          Format.flush_str_formatter ()
        with exn ->
          sprintf "rwo_runtop: the following error was raised during \
                   error reporting for %S:\n%s\nError backtrace:\n%s\n%!"
            input (Exn.to_string exn) (Printexc.get_backtrace ())
      in
      if not backtrace_enabled then Printexc.record_backtrace false;
      {Code.input; output; stdout; stderr}
    )
  )


(*** Suppress values beginning with _.  Lifted straight from uTop:
 * uTop_main.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 **)

let orig_print_out_signature = !Toploop.print_out_signature
let orig_print_out_phrase = !Toploop.print_out_phrase

let rec map_items
    (unwrap : 'a -> Outcometree.out_sig_item * 'b)
    (wrap : Outcometree.out_sig_item -> 'b -> 'a)
    (items : 'a list)
    : 'a list
    =
  match items with
  | [] ->
    []
  | item :: items ->
    let sig_item, _ = unwrap item in
    let name, _ =
      match sig_item with
      | Outcometree.Osig_class (_, name, _, _, rs)
      | Outcometree.Osig_class_type (_, name, _, _, rs)
      | Outcometree.Osig_module (name, _, rs)
      | Outcometree.Osig_type ({Outcometree.otype_name=name;_}, rs) ->
        (name, rs)
      | Outcometree.Osig_typext ({Outcometree.oext_name=name;_}, _)
      | Outcometree.Osig_modtype (name, _)
      | Outcometree.Osig_value (name, _, _) ->
        (name, Outcometree.Orec_not)
    in
    let keep = name = "" || name.[0] <> '_' in
    if keep then
      item :: map_items unwrap wrap items
    else
      (* Replace the [Orec_next] at the head of items by [Orec_first] *)
      let items =
        match items with
        | [] ->
          []
        | item :: items' ->
          let sig_item, extra = unwrap item in
          match sig_item with
          | Outcometree.Osig_class (a, name, b, c, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_class (a, name, b, c, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_class_type (a, name, b, c, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_class_type (a, name, b, c, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_module (name, a, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_module (name, a, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_type (out_type_decl, rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_type (out_type_decl, Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_typext _
          | Outcometree.Osig_modtype _
          | Outcometree.Osig_value _ ->
            items
      in
      map_items unwrap wrap items

let print_out_signature (pp:Format.formatter) (items : Outcometree.out_sig_item list) : unit =
  orig_print_out_signature pp (map_items (fun x -> (x, ())) (fun x () -> x) items)

let print_out_phrase (pp:Format.formatter) (phrase:Outcometree.out_phrase) : unit =
  let phrase =
    match phrase with
    | Outcometree.Ophr_eval _
    | Outcometree.Ophr_exception _ -> phrase
    | Outcometree.Ophr_signature items ->
      Outcometree.Ophr_signature (map_items (fun x -> x) (fun x y -> (x, y)) items)
  in
  orig_print_out_phrase pp phrase

let () =
  Toploop.print_out_signature := print_out_signature;
  Toploop.print_out_phrase := print_out_phrase

(** End of uTop code *)

let run ?out_dir filename =
  eprintf "C: %s\n%!" filename;
  let out_dir = match out_dir with
    | Some x -> x
    | None -> Filename.dirname filename
  in
  reset_toplevel ();
  List.iter initial_phrases ~f:(fun phrase ->
    let p = toploop_eval (phrase ^ " ;;") in
    match p.Code.output with
    | "" -> ()
    | _ -> eprintf "ERROR: %s\n%!" p.Code.output
  );
  In_channel.read_all filename
  |> Code.split_parts_exn ~lang:`OCaml_toplevel ~filename
  |> List.iter ~f:(fun (part,content) ->
    eprintf "X: %s, part %d\n%s\n\n%!" filename part content;
    let data =
      ok_exn (Code.split_ocaml_toplevel_phrases `Anywhere content)
      |> List.map ~f:toploop_eval
      |> <:sexp_of< Code.phrase list >>
      |> Sexp.to_string
    in
    let base = Filename.(basename filename |> chop_extension) in
    let out_file = sprintf "%s/%s.%d.sexp" out_dir base part in
    Out_channel.write_all out_file ~data
  )


let main : Command.t = Command.basic
  ~summary:"Run files through the Core toplevel"
  Command.Spec.(
    empty
    +> flag "-o" (optional string)
      ~doc:"DIR Write files to directory DIR. Default is write to the \
                same directory that FILE is in."
    +> anon (sequence ("file" %: file))
  )
  (fun out_dir files () -> List.iter files ~f:(run ?out_dir))

let () = Command.run main

(* Syntax hightlight code and eval ocaml toplevel phrases.
 * Based on http://github.com/ocaml/ocaml.org 
 * Modified by Anil Madhavapeddy for Real World OCaml and to use Core *)
open Printf
open Scanf

(* Run these phrases silently before any code *)
let initial_phrases = [
  "#use \"topfind\"";
  "#camlp4o";
  "#require \"core\"";
  "#require \"core.syntax\"";
  "#require \"core.top\"";
  "open Core.Std" ]

(* Initialise toploop and turn on short-paths *)
let reset_toplevel () =
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := ""; (* no filename *)
  Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH");
  Clflags.real_paths := false

let build_dir = ref "."
let ofile file part = sprintf "%s/%s.%d.md" !build_dir file part
let ofile_html file part = sprintf "%s/%s.%d.html" !build_dir file part

type outcome = [
  | `Normal of string * string * string (* exec output, stdout, stderr *)
  | `Error of string
]

let is_ready_for_read fd =
  let fd_for_read, _, _ = Unix.select [fd] [] [] 0.001 in
  fd_for_read <> []

let string_of_fd fd =
  let buf = Buffer.create 1024 in
  let s = String.create 256 in
  while is_ready_for_read fd do
    let r = Unix.read fd s 0 256 in
    Buffer.add_substring buf s 0 r
  done;
  Buffer.contents buf

let init_stdout = Unix.dup Unix.stdout
let init_stderr = Unix.dup Unix.stderr

let flush_std_out_err () =
  Format.pp_print_flush Format.std_formatter ();
  flush stdout;
  Format.pp_print_flush Format.err_formatter ();
  flush stderr

let toploop_eval phrase =
  if String.trim phrase = ";;" then `Normal("", "", "")
  else (
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
      let lexbuf = Lexing.from_string phrase in
      let phrase = !Toploop.parse_toplevel_phrase lexbuf in
      ignore(Toploop.execute_phrase true Format.str_formatter phrase);
      let exec_output = Format.flush_str_formatter () in
      let out, err = get_stdout_stderr_and_restore () in
      `Normal(exec_output, out, err)
    with
    | e ->
      let out, err = get_stdout_stderr_and_restore () in
      print_string out;
      prerr_string err;
      let backtrace_enabled = Printexc.backtrace_status () in
      if not backtrace_enabled then Printexc.record_backtrace true;
      (try Errors.report_error Format.str_formatter e
       with exn ->
         printf "Code.toploop_eval: the following error was raised during \
                 error reporting for %S:\n%s\nError backtrace:\n%s\n%!"
           phrase (Printexc.to_string exn) (Printexc.get_backtrace ());
      );
      if not backtrace_enabled then Printexc.record_backtrace false;
      `Error(Format.flush_str_formatter ())
  )

open Core.Std
let parse_file file =
  eprintf "C: init\n%!";
  reset_toplevel ();
  List.iter initial_phrases ~f:(fun phrase ->
      match toploop_eval (phrase ^ " ;;") with
      | `Normal _ -> ()
      | `Error s -> eprintf "Failed (%s): %s\n" s phrase; exit (-1)
    );
  let parts = Int.Table.create () in
  let html_parts = Int.Table.create () in
  let part = ref 0 in
  let print_part key s =
    match Hashtbl.find parts key with
    | None ->
      let buf = Buffer.create 100 in
      Hashtbl.replace parts ~key ~data:buf;
      Buffer.add_string buf s
    | Some buf -> Buffer.add_string buf s
  in
  let print_html_part key s =
    match Hashtbl.find html_parts key with
    | None ->
      let buf = Buffer.create 100 in
      Hashtbl.replace html_parts ~key ~data:buf;
      Buffer.add_string buf s
    | Some buf -> Buffer.add_string buf s
  in

  let _ =
    In_channel.with_file file ~f:(
      In_channel.fold_lines ~init:[] ~f:(
        fun acc line ->
          if String.is_prefix line ~prefix:"#part" then begin
            part := Scanf.sscanf line "#part %d" (fun p -> p);
            eprintf "C: part %d -> %s\n%!" !part (ofile file !part); 
            []
          end else begin
            if String.is_suffix ~suffix:";;" line then begin
              let phrase = String.concat ~sep:"\n" (List.rev (line :: acc)) in
              eprintf "X: %s\n%!" phrase;
              match toploop_eval phrase with
              | `Normal(s, _, _) | `Error s ->
                print_part !part (sprintf "# %s \n%s" phrase s);
                print_html_part !part (Cow.Html.to_string (Cow.Code.ocaml_fragment ("# " ^ phrase)));
                if s <> "" then print_html_part !part (Cow.Html.to_string <:html<<div class="rwocodeout">$str:s$</div>&>>);
                []
            end else
              line::acc
          end
      );
    ) in
  Hashtbl.iter parts ~f:(
    fun ~key ~data ->
      eprintf "W: %s\n%!" (ofile file key);
      Out_channel.write_all (ofile file key) ~data:(Buffer.contents data));
  Hashtbl.iter html_parts ~f:(
    fun ~key ~data ->
      let data = sprintf "<div class=\"ocaml\"><pre><code>%s</code></pre></div>" (String.strip (Buffer.contents data)) in
      eprintf "W: %s\n%!" (ofile_html file key);
      Out_channel.write_all (ofile_html file key) ~data)

let () =
  Command.basic
    ~summary:"Run files through the Core toplevel"
    Command.Spec.(empty 
                  +> flag "-builddir" (optional_with_default "." string)
                      ~doc:"dir prepend build directory to output files"
                  +> anon (sequence ("file" %: file))
                 )
    (fun bd files () -> build_dir := bd; List.iter ~f:parse_file files)
  |> Command.run

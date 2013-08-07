(* Syntax hightlight code and eval ocaml toplevel phrases.
 * Based on code from http://github.com/ocaml/ocaml.org 
 * Modified by Anil Madhavapeddy in 2013 for Real World OCaml and to use Core
 * Much code borrowed from uTop.
 * TODO: license?
 *)

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
  Toploop.input_name := "//toplevel//";
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

let with_loc loc str = {
  Location.txt = str;
  Location.loc = loc;
}

(** Rewrite rules, lifted straight from uTop. *)

(* A rule for rewriting a toplevel expression. *)
type rewrite_rule = {
  required_values : Longident.t list;
  (* Values that must exist and be persistent for the rule to apply. *)
  rewrite : Location.t -> Parsetree.expression -> Parsetree.expression;
  (* The rewrite function. *)
}

(* Rewrite rules, indexed by the identifier of the type
   constructor. *)
let rewrite_rules : (Longident.t, rewrite_rule) Hashtbl.t = Hashtbl.create 42

let longident_lwt_main_run = Longident.Ldot (Longident.Lident "Lwt_main", "run")
let longident_async_core_thread_safe_block_on_async_exn =
  Longident.parse "Async.Std.Thread_safe.block_on_async_exn"
let longident_unit = Longident.Lident "()"

(* Wrap <expr> into: fun () -> <expr> *)
let wrap_unit loc e =
  let i = with_loc loc longident_unit in
  let p = {
    Parsetree.ppat_desc = Parsetree.Ppat_construct (i, None, false);
    Parsetree.ppat_loc = loc;
  } in
  {
    Parsetree.pexp_desc = Parsetree.Pexp_function ("", None, [(p, e)]);
    Parsetree.pexp_loc = loc;
  }

let () =
  (* Rewrite Async.Std.Defered.t expressions to
     Async_core.Thread_safe.block_on_async_exn (fun () -> <expr>). *)
  Hashtbl.add rewrite_rules (Longident.parse "Async_core.Ivar.Deferred.t") {
    required_values = [longident_async_core_thread_safe_block_on_async_exn];
    rewrite = (fun loc e -> {
      Parsetree.pexp_desc =
        Parsetree.Pexp_apply
          ({ Parsetree.pexp_desc = Parsetree.Pexp_ident
              (with_loc loc longident_async_core_thread_safe_block_on_async_exn);
             Parsetree.pexp_loc = loc },
           [("", wrap_unit loc e)]);
      Parsetree.pexp_loc = loc;
    });
  }

(* Returns whether the argument is a toplevel expression. *)
let is_eval = function
  | { Parsetree.pstr_desc = Parsetree.Pstr_eval _ } -> true
  | _ -> false

(* Returns whether the given path is persistent. *)
let rec is_persistent_path = function
  | Path.Pident id -> Ident.persistent id
  | Path.Pdot (p, _, _) -> is_persistent_path p
  | Path.Papply (_, p) -> is_persistent_path p

(* Convert a path to a long identifier. *)
let rec longident_of_path path =
  match path with
    | Path.Pident id ->
      Longident.Lident (Ident.name id)
    | Path.Pdot (path, s, _) ->
      Longident.Ldot (longident_of_path path, s)
    | Path.Papply (p1, p2) ->
      Longident.Lapply (longident_of_path p1, longident_of_path p2)

(* Returns the rewrite rule associated to a type, if any. *)
let rec rule_of_type typ =
  match typ.Types.desc with
    | Types.Tlink typ ->
      rule_of_type typ
    | Types.Tconstr (path, _, _) -> begin
      match try Some (Env.find_type path !Toploop.toplevel_env) with Not_found -> None with
      | Some {
        Types.type_kind = Types.Type_abstract;
        Types.type_private = Asttypes.Public;
        Types.type_manifest = Some typ;
      } ->
        rule_of_type typ
      | _ ->
        try
          Some (Hashtbl.find rewrite_rules (longident_of_path path))
        with Not_found ->
          None
    end
    | _ ->
      None

(* Check that the given long identifier is present in the environment
   and is persistent. *)
let is_persistent_in_env longident =
  try
    is_persistent_path (fst (Env.lookup_value longident !Toploop.toplevel_env))
  with Not_found ->
    false

let str_items_of_typed_structure tstr = tstr.Typedtree.str_items
let str_desc_of_typed_str_item tstr = tstr.Typedtree.str_desc

let rewrite_str_item pstr_item tstr_item =
  match pstr_item, str_desc_of_typed_str_item tstr_item with
    | ({ Parsetree.pstr_desc = Parsetree.Pstr_eval e;
         Parsetree.pstr_loc = loc },
       Typedtree.Tstr_eval { Typedtree.exp_type = typ }) -> begin
      match rule_of_type typ with
        | Some rule ->
          if List.for_all is_persistent_in_env rule.required_values then begin
            { Parsetree.pstr_desc = Parsetree.Pstr_eval (rule.rewrite loc e);
              Parsetree.pstr_loc = loc }
          end else
            pstr_item
        | None ->
          pstr_item
    end
    | _ ->
      pstr_item

let rewrite phrase =
  match phrase with
    | Parsetree.Ptop_def pstr ->
      if List.exists is_eval pstr then
        let tstr, _, _ = Typemod.type_structure !Toploop.toplevel_env pstr Location.none in
        let tstr = str_items_of_typed_structure tstr in
        Parsetree.Ptop_def (List.map2 rewrite_str_item pstr tstr)
      else begin
        phrase
      end
    | Parsetree.Ptop_dir _ ->
      phrase

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
      let dummypos = { Lexing.pos_fname = "//toplevel//"; pos_lnum = 0; pos_bol = 0; pos_cnum = -1; } in
      lexbuf.Lexing.lex_start_p <- dummypos;
      lexbuf.Lexing.lex_curr_p <- dummypos;
      let phrase = !Toploop.parse_toplevel_phrase lexbuf in
      let phrase = rewrite phrase in
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


(*** Suppress values beginning with _.  Lifted straight from uTop:
 * uTop_main.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 **)

let orig_print_out_signature = !Toploop.print_out_signature
let orig_print_out_phrase = !Toploop.print_out_phrase

let rec map_items unwrap wrap items =
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
      | Outcometree.Osig_type ((name, _, _, _, _), rs) ->
        (name, rs)
      | Outcometree.Osig_exception (name, _)
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
          | Outcometree.Osig_type ((name, a, b, c, d), rs) ->
            if rs = Outcometree.Orec_next then
              wrap (Outcometree.Osig_type ((name, a, b, c, d), Outcometree.Orec_first)) extra :: items'
            else
              items
          | Outcometree.Osig_exception _
          | Outcometree.Osig_modtype _
          | Outcometree.Osig_value _ ->
            items
      in
      map_items unwrap wrap items

let print_out_signature pp items =
  orig_print_out_signature pp (map_items (fun x -> (x, ())) (fun x () -> x) items)

let print_out_phrase pp phrase =
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

open Core.Std
let parse_file fullfile file =
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
              | `Normal(s, stdout, stderr) ->
                print_part !part (sprintf "# %s \n%s%s%s"
                  phrase
                  (if stdout = "" then "" else "\n"^stdout)
                  (if stderr = "" then "" else "\n"^stderr)
                  s
                );
                print_html_part !part (Cow.Html.to_string (Cow.Code.ocaml_fragment ("# " ^ phrase)));
                let sout = if stdout = "" then <:html<&>> else <:html<<br />$str:stdout$>> in
                let serr = if stderr = "" then <:html<&>> else <:html<<br />$str:stderr$>> in
                let s = if s ="" then " " else s in
                print_html_part !part (Cow.Html.to_string <:html<<div class="rwocodeout">$sout$$serr$$str:s$</div>&>>);
                []
              | `Error s ->
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
      let code = Cow.Html.of_string (String.strip (Buffer.contents data)) in
      let data =
        Code_frag.wrap_in_pretty_box ~part:key "OCaml UTop" fullfile code
        |> Cow.Html.to_string in
      eprintf "W: %s\n%!" (ofile_html file key);
      Out_channel.write_all (ofile_html file key) ~data)

let () =
  Command.basic
    ~summary:"Run files through the Core toplevel"
    Command.Spec.(empty 
                  +> flag "-builddir" (optional_with_default "." string)
                      ~doc:"dir prepend build directory to output files"
                  +> flag "-fullfile" (required string)
                      ~doc:"filename full subdir/filename for prettyprinting in HTML"
                  +> anon (sequence ("file" %: file))
                 )
    (fun bd fullfile files () -> build_dir := bd; List.iter ~f:(parse_file fullfile) files)
  |> Command.run

module Linked = struct
  include Topdirs
  include Ephemeron
  include Uchar
end

open Lexing
open Parsetree

(** {1 Phrase parsing} *)

type phrase = {
  startpos: position;
  endpos: position;
  parsed: (toplevel_phrase, exn) result;
}

let toplevel_fname = "//toplevel//"

let shift_toplevel_position start pos = {
  pos_fname = toplevel_fname;
  pos_lnum = pos.pos_lnum - start.pos_lnum + 1;
  pos_bol  = pos.pos_bol  - start.pos_cnum - 1;
  pos_cnum = pos.pos_cnum - start.pos_cnum - 1;
}

let shift_toplevel_location start loc =
  let open Location in
  {loc with loc_start = shift_toplevel_position start loc.loc_start;
            loc_end = shift_toplevel_position start loc.loc_end}

let shift_location_error start =
  let open Location in
  let rec aux (error : Location.error) =
    {error with sub = List.map aux error.sub;
                loc = shift_toplevel_location start error.loc}
  in
  aux

let position_mapper start =
  let open Ast_mapper in
  let location mapper loc =
    shift_toplevel_location start (default_mapper.location mapper loc)
  in
  {default_mapper with location}

let initial_pos = {
  pos_fname = toplevel_fname;
  pos_lnum  = 1;
  pos_bol   = 0;
  pos_cnum  = 0;
}

let semisemi_action =
  let lexbuf = Lexing.from_string ";;" in
  match Lexer.token lexbuf with
  | Parser.SEMISEMI ->
    lexbuf.Lexing.lex_last_action
  | _ -> assert false

let init_parser ~fname contents =
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- {initial_pos with pos_fname = fname};
  Location.input_name := fname;
  (contents, lexbuf)

(* Take a part of a file, trimming spaces at the beginning as well as ';;' *)
let sub_file file_contents ~start ~stop =
  let rec loop start =
    if start >= stop then
      start
    else
      match file_contents.[start] with
      | ' ' | '\t' | '\n' -> loop (start + 1)
      | ';' when start + 1 < stop && file_contents.[start+1] = ';' ->
        loop (start + 2)
      | _ -> start
  in
  let start = loop start in
  String.sub file_contents start (stop - start)
;;


let parse_phrase (contents, lexbuf) =
  let open Lexing in
  let startpos = lexbuf.Lexing.lex_curr_p in
  let parsed = match Parse.toplevel_phrase lexbuf with
    | phrase -> Ok phrase
    | exception exn ->
      let exn = match Location.error_of_exn exn with
        | None -> raise exn
        | Some error -> Location.Error (shift_location_error startpos error)
      in
      if lexbuf.Lexing.lex_last_action <> semisemi_action then begin
        let rec aux () = match Lexer.token lexbuf with
          | Parser.SEMISEMI | Parser.EOF -> ()
          | _ -> aux ()
        in
        aux ();
      end;
      Error exn
  in
  let endpos = lexbuf.Lexing.lex_curr_p in
  { startpos; endpos; parsed }

(** *)

type 'a phrase_role =
  | Phrase_code of 'a
  | Phrase_expect of string
  | Phrase_part of string

let payload_constant = function
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant const}, _); _}] ->
    Some const
  | _ -> None

let payload_string = function
  | PStr [] -> Some ""
  | x -> match payload_constant x with
    | Some (Pconst_string (str, _)) -> Some str
    | _ -> None

let constant_payload const = PStr [Ast_helper.(Str.eval (Exp.constant const))]
let string_payload x = constant_payload (Pconst_string (x, None))

let phrase_role phrase = match phrase.parsed with
  | Ok (Ptop_def [{pstr_desc = Pstr_extension((name, payload), attrs); pstr_loc = loc}])
      when name.Asttypes.txt = "expect" && payload_string payload <> None ->
    begin match payload_string payload with
      | None -> assert false
      | Some const -> Phrase_expect const
    end
  | Ok (Ptop_def [{pstr_desc = Pstr_attribute (name, payload); pstr_loc = loc}])
      when name.Asttypes.txt = "part" && payload_string payload <> None ->
    begin match payload_string payload with
      | None -> Phrase_code ()
      | Some part -> Phrase_part part
    end
  | _ -> Phrase_code ()

let verbose = ref false
let () = Hashtbl.add Toploop.directive_table "verbose"
    (Toploop.Directive_bool (fun x -> verbose := x))
;;

let toplevel_exec_phrase ppf = function
  | { parsed = Error exn; _} -> raise exn
  | { parsed = Ok phrase; startpos; _} ->
    let mapper = position_mapper {startpos with pos_fname = toplevel_fname} in
    let phrase = match phrase with
      | Ptop_def str -> Ptop_def (mapper.Ast_mapper.structure mapper str)
      | Ptop_dir _ as x -> x
    in
    let phrase = match phrase with
      | Ptop_dir _ as x -> x
      | Ptop_def s -> Ptop_def (Pparse.apply_rewriters_str ~tool_name:"expect" s)
    in
    if !Clflags.dump_parsetree then Printast. top_phrase ppf phrase;
    if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
    Toploop.execute_phrase !verbose ppf phrase
;;

type var_and_value = V : 'a ref * 'a -> var_and_value

let protect_vars =
  let set_vars l = List.iter (fun (V (r, v)) -> r := v) l in
  fun vars ~f ->
    let backup = List.map (fun (V (r, _)) -> V (r, !r)) vars in
    set_vars vars;
    Misc.try_finally f (fun () -> set_vars backup)
;;

let capture_compiler_stuff ppf ~f =
  protect_vars
    [ V (Location.formatter_for_warnings , ppf) ]
    ~f
;;

let redirect ~f =
  let stdout_backup = Unix.dup Unix.stdout in
  let stderr_backup = Unix.dup Unix.stdout in
  let filename = Filename.temp_file "expect-test" "stdout" in
  let fd_out = Unix.openfile filename Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.dup2 fd_out Unix.stderr;
  let ic = open_in filename in
  let read_up_to = ref 0 in
  let capture buf =
    flush stdout;
    flush stderr;
    let pos = Unix.lseek fd_out 0 Unix.SEEK_CUR in
    let len = pos - !read_up_to in
    read_up_to := pos;
    Buffer.add_channel buf ic len
  in
  Misc.try_finally (fun () -> f ~capture)
    (fun () ->
       close_in_noerr ic;
       Unix.close fd_out;
       Unix.dup2 stdout_backup Unix.stdout;
       Unix.dup2 stderr_backup Unix.stderr;
       Unix.close stdout_backup;
       Unix.close stderr_backup;
       Sys.remove filename)
;;

let cleanup_singleline str =
  if str = "" then str
  else
    let trim_from = if str.[0] = '\n' then 1 else 0 in
    let len = String.length str in
    match String.index_from str 1 '\n' with
    | exception Not_found ->
      String.sub str trim_from (len - trim_from)
    | x when x = len - 1 ->
      String.sub str trim_from (x - trim_from)
    | _ -> str

let eval_phrases ~fname fcontents =
  (* 4.03: Warnings.reset_fatal (); *)
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let exec_phrase ~capture phrase =
    match phrase_role phrase with
    | (Phrase_expect _ | Phrase_part _) as x -> x
    | Phrase_code () ->
      (* So that [%expect_exact] nodes look nice *)
      Buffer.add_char buf '\n';
      begin match toplevel_exec_phrase ppf phrase with
        | (_ : bool) -> ()
        | exception exn ->
          Location.report_exception ppf exn
      end;
      Format.pp_print_flush ppf ();
      let len = Buffer.length buf in
      if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
        (* So that [%expect_exact] nodes look nice *)
        Buffer.add_char buf '\n';
      capture buf;
      if Buffer.nth buf (len - 1) <> '\n' then
        Buffer.add_char buf '\n';
      let s = Buffer.contents buf in
      Buffer.clear buf;
      Phrase_code (cleanup_singleline s)
  in
  let parser = init_parser ~fname fcontents in
  redirect ~f:(fun ~capture ->
      capture_compiler_stuff ppf ~f:(fun () ->
          let rec aux chunks = match parse_phrase parser with
            | exception End_of_file ->  List.rev chunks
            | phrase ->
              let role = exec_phrase ~capture phrase in
              aux ((phrase, role) :: chunks)
          in
          aux []
        )
    )
;;

(*let rec chunks_of_phrases part = function
  | [] -> []
  | (p, Phrase_code outcome) :: (p', Phrase_expect expectation) :: rest ->
    let phrases = match p.parsed with Ok p -> [p] | Error _ -> [] in
    let t = { part; phrases; expectation; phrases_loc = phrase_loc p } in
    t :: chunks_of_phrases part rest
  | [(p, Phrase_part part)] ->
    [{ part; phrases = []; expectation = assert false;
       phrases_loc = phrase_loc p }]
  | (_, Phrase_part part) :: rest ->
    chunks_of_phrases part rest
  | (p, Phrase_code outcome) :: rest ->
    let phrases = match p.parsed with Ok p -> [p] | Error _ -> [] in
    let t = { part; phrases; phrases_loc = phrase_loc p;
              expectation = no_expectation } in
    t :: chunks_of_phrases part rest
  | (p, Phrase_expect expectation) :: rest ->
    let t = { part; phrases = []; expectation; phrases_loc = phrase_loc p } in
    t :: chunks_of_phrases part rest
*)

let is_whitespace str =
  try for i = 0 to String.length str - 1 do
      match str.[i] with
      | ' ' | '\n' -> ()
      | _ -> raise Exit
    done;
    true
  with Exit -> false

let rec valid_phrases = function
  | [] -> true
  | (_, Phrase_part _) :: rest -> valid_phrases rest
  | (_, Phrase_code outcome) :: (_, Phrase_expect outcome') :: rest ->
    outcome = outcome' && valid_phrases rest
  | (_, Phrase_code outcome) :: rest ->
    is_whitespace outcome && valid_phrases rest
  | (_, Phrase_expect _) :: _ -> false

let phrase_code contents phrase =
  let start = phrase.startpos.pos_cnum in
  let stop = phrase.endpos.pos_cnum in
  sub_file contents ~start ~stop

let phrase_whitespace contents phrase rest =
  let start = phrase.endpos.pos_cnum in
  let stop = match rest with
    | [] -> String.length contents
    | (phrase', _) :: _ -> phrase'.startpos.pos_cnum
  in
  String.sub contents start (stop - start)

let output_phrases oc contents =
  let rec aux = function
    | [] -> ()
    | (_, Phrase_part x) :: rest ->
      Printf.fprintf oc "[@@@part %S];;\n" x;
      aux rest
    | (phrase, Phrase_code expect_code) :: rest ->
      let phrase_ws, expect_ws = match rest with
        | (phrase_expect, Phrase_expect _) :: rest' ->
          (phrase_whitespace contents phrase rest,
           phrase_whitespace contents phrase_expect rest')
        | _ ->
          ("\n", phrase_whitespace contents phrase rest)
      in
      let phrase_code = phrase_code contents phrase in
      if is_whitespace expect_code then
        Printf.fprintf oc "%s%s" phrase_code expect_ws
      else
        Printf.fprintf oc "%s%s[%%%%expect{|%s|}];;%s"
          phrase_code phrase_ws expect_code expect_ws;
      aux rest
    | (_, Phrase_expect _) :: rest ->
      aux rest
  in aux

let document_of_phrases contents matched phrases =
  let open Toplevel_expect_test_types in
  let rec parts_of_phrase part acc = function
    | (_, Phrase_part part') :: rest ->
      { Part. name = part; chunks = List.rev acc } ::
      parts_of_phrase part' [] rest
    | (_, Phrase_expect _) :: rest ->
      parts_of_phrase part acc rest
    | (phrase, Phrase_code toplevel_response) :: rest ->
      let ocaml_code = phrase_code contents phrase in
      let chunk = {Chunk. ocaml_code; toplevel_response} in
      parts_of_phrase part (chunk :: acc) rest
    | [] ->
      if part <> "" || acc <> [] then
        [{ Part. name = part; chunks = List.rev acc }]
      else
        []
  in
  let parts = parts_of_phrase "" [] phrases in
  { Document. matched; parts }

let process_expect_file ~fname ~use_color ~in_place ~sexp_output =
  let file_contents =
    let ic = open_in fname in
    let len = in_channel_length ic in
    let result = really_input_string ic len in
    close_in_noerr ic;
    result
  in
  let phrases = eval_phrases ~fname file_contents in
  let success = valid_phrases phrases in
  let oname = if in_place then fname else fname ^ ".corrected" in
  if success && not in_place && Sys.file_exists oname then
    Sys.remove oname;
  if not success then (
    let oc = open_out_bin (oname ^ ".tmp") in
    output_phrases oc file_contents phrases;
    flush oc;
    close_out oc;
    Sys.rename (oname ^ ".tmp") oname
  );
  if sexp_output then (
    document_of_phrases file_contents success phrases
    |> Toplevel_expect_test_types.Document.sexp_of_t
    |> Sexplib.Sexp.output stdout
  );
  success
;;

let override_sys_argv args =
  let len = Array.length args in
  assert (len <= Array.length Sys.argv);
  Array.blit args 0 Sys.argv 0 len;
  Obj.truncate (Obj.repr Sys.argv) len;
  Arg.current := 0;
;;

let use_color   = ref true
let in_place    = ref false
let sexp_output = ref false

let process_file fname =
  let cmd_line =
    Array.sub Sys.argv !Arg.current (Array.length Sys.argv - !Arg.current)
  in
  override_sys_argv cmd_line;
  Toploop.set_paths ();
  Compmisc.init_path true;
  Toploop.toplevel_env := Compmisc.initial_env ();
  Sys.interactive := false;
  let success =
    process_expect_file ~fname ~use_color:!use_color ~in_place:!in_place
      ~sexp_output:!sexp_output
  in
  exit (if success then 0 else 1)
;;

let args =
  Arg.align
    [ "-in-place", Arg.Set in_place,    " Overwrite file in place"
    ; "-sexp"    , Arg.Set sexp_output, " Output the result as a s-expression instead of diffing"
    ; "-verbose" , Arg.Set verbose, " Include outcome of phrase evaluation (like ocaml toplevel)"
    ]

let print_version () =
  Printf.printf "topexect, version %s\n" Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  Printf.printf "%s\n" Sys.ocaml_version;
  exit 0;
;;

module Options = Main_args.Make_bytetop_options (struct
    open Clflags
    open Compenv

    let set r () = r := true
    let clear r () = r := false

    let _absname = set Location.absname
    let _I dir =
      let dir = Misc.expand_directory Config.standard_library dir in
      include_dirs := dir :: !include_dirs
    let _init s = init_file := Some s
    let _noinit = set noinit
    let _labels = clear classic
    let _alias_deps = clear transparent_modules
    let _no_alias_deps = set transparent_modules
    let _app_funct = set applicative_functors
    let _no_app_funct = clear applicative_functors
    let _noassert = set noassert
    let _nolabels = set classic
    let _noprompt = set noprompt
    let _nopromptcont = set nopromptcont
    let _nostdlib = set no_std_include
    let _open s = open_modules := s :: !open_modules
    let _plugin p = Compplugin.load p
    let _ppx s = first_ppx := s :: !first_ppx
    let _principal = set principal
    let _no_principal = clear principal
    let _rectypes = set recursive_types
    let _no_rectypes = clear recursive_types
    let _safe_string = clear unsafe_string
    let _short_paths = clear real_paths
    let _stdin () = raise (Arg.Bad "-stdin not supported")
    let _strict_sequence = set strict_sequence
    let _no_strict_sequence = clear strict_sequence
    let _strict_formats = set strict_formats
    let _no_strict_formats = clear strict_formats
    let _unboxed_types = set unboxed_types
    let _no_unboxed_types = clear unboxed_types
    let _unsafe = set fast
    let _unsafe_string = set unsafe_string
    let _version () = print_version ()
    let _vnum () = print_version_num ()
    let _no_version = set noversion
    let _w s = Warnings.parse_options false s
    let _warn_error s = Warnings.parse_options true s
    let _warn_help = Warnings.help_warnings
    let _dparsetree = set dump_parsetree
    let _dtypedtree = set dump_typedtree
    let _dsource = set dump_source
    let _drawlambda = set dump_rawlambda
    let _dlambda = set dump_lambda
    let _dflambda = set dump_flambda
    let _dtimings = set print_timings
    let _dinstr = set dump_instr

    let anonymous s = process_file s
  end);;



let main () =
  let usage =
    Printf.sprintf "Usage: %s [OPTIONS] FILE [ARGS]\n"
      (Filename.basename Sys.argv.(0))
  in
  try
    Arg.parse (args @ Options.list) process_file (usage ^ "\nOptions are:");
    prerr_endline usage;
    exit 2
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
;;
let () = main ()

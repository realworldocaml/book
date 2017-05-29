module Linked = struct
  include Topdirs
  include Ephemeron
  include Uchar
end

open Lexing
open Parsetree
open Sexplib.Conv

(* Standard outputs should be disable: multi threaded code could print
 * at anytime, so we just disable output by defaul. *)

let stdout_backup = Unix.out_channel_of_descr (Unix.dup Unix.stdout)
let stderr_backup = Unix.out_channel_of_descr (Unix.dup Unix.stderr)

let prerr_endline str =
  output_string stderr_backup str;
  output_char stderr_backup '\n';
  flush stderr_backup

let disable_outputs = lazy (
  let fd_out = Unix.openfile "/dev/null" Unix.[O_WRONLY] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.dup2 fd_out Unix.stderr;
  Unix.close fd_out;
)


module Chunk = struct
  type kind = OCaml | Raw
    [@@deriving sexp]

  type response = (kind * string)
    [@@deriving sexp]

  type t =
    { ocaml_code : string; toplevel_responses : response list; }
    [@@deriving sexp]

  let code c = c.ocaml_code
  let warnings (_ : t) : string =  ""
  let responses c = c.toplevel_responses
  let stdout (_ : t) = ""
  let evaluated (_ : t) = true
end

module Part = struct
  type t =
    { name : string; chunks : Chunk.t list; }
    [@@deriving sexp]
end

module Document = struct
  type t =
    { parts : Part.t list; matched : bool; }
    [@@deriving sexp]
end

(** {1 Phrase parsing} *)

type phrase = {
  startpos : position;
  endpos   : position;
  parsed   : (toplevel_phrase, exn) result;
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
  | Phrase_expect of Chunk.response list
  | Phrase_part of string

exception Cannot_parse_payload of Location.t

let string_of_location {Location.loc_start = {pos_fname; pos_lnum; pos_bol; pos_cnum}} =
  Printf.sprintf "%s, line %d, col %d" pos_fname pos_lnum (pos_cnum - pos_bol)

let payload_constants loc = function
  | PStr [{pstr_desc = Pstr_eval (expr, _); _}] ->
    let one {pexp_loc; pexp_desc} = match pexp_desc with
      | Pexp_apply ({pexp_desc = Pexp_ident ident},
                    [Asttypes.Nolabel, {pexp_desc = Pexp_constant const}]) ->
        (pexp_loc, Some ident, const)
      | Pexp_constant const -> (pexp_loc, None, const)
      | _ -> raise (Cannot_parse_payload pexp_loc)
    in
    let rec consts = function
      | {pexp_desc=Pexp_sequence(e, rest)} -> one e :: consts rest
      | e -> [one e]
    in
    consts expr
  | PStr [] -> []
  | _ -> raise (Cannot_parse_payload loc)

let payload_strings loc = function
  | PStr [] -> []
  | x ->
    let aux = function
      | _, Some {Location.txt = Longident.Lident "ocaml"},
        Pconst_string (str, _) -> (Chunk.OCaml, str)
      | _, None, Pconst_string (str, _) -> (Chunk.Raw, str)
      | loc, _, _ -> raise (Cannot_parse_payload loc)
    in
    List.map aux (payload_constants loc x)

let constant_payload const = PStr [Ast_helper.(Str.eval (Exp.constant const))]
let string_payload x = constant_payload (Pconst_string (x, None))

let phrase_role phrase = match phrase.parsed with
  | Ok (Ptop_def [{pstr_desc = Pstr_extension((name, payload), attrs); pstr_loc}])
    when name.Asttypes.txt = "expect" ->
    begin match payload_strings pstr_loc payload with
      | x -> Phrase_expect x
      | exception (Cannot_parse_payload loc) ->
        prerr_endline (string_of_location loc ^ ": cannot parse [%%expect] payload");
        Phrase_code ()
   end
  | Ok (Ptop_def [{pstr_desc = Pstr_attribute (name, payload); pstr_loc}])
    when name.Asttypes.txt = "part" ->
    begin match payload_strings pstr_loc payload with
      | [Chunk.Raw, part] -> Phrase_part part
      | _ ->
        prerr_endline (string_of_location pstr_loc ^ ": cannot parse [@@@part] payload");
        Phrase_code ()
      | exception (Cannot_parse_payload loc) ->
        prerr_endline
          (string_of_location loc ^ ": cannot parse [@@@part] payload");
        Phrase_code ()
    end
  | _ -> Phrase_code ()

let verbose = ref false
let () = Hashtbl.add Toploop.directive_table "verbose"
    (Toploop.Directive_bool (fun x -> verbose := x))
;;

module Async_autorun = struct
  (* Inspired by Utop auto run rewriter *)
  let (async_typ, async_runner, async_rewrite) =
    let typ = Longident.parse "Async.Deferred.t" in
    let runner = Longident.parse "Async.Thread_safe.block_on_async_exn" in
    let open Ast_helper in
    let rewrite loc e =
      let punit =
        Pat.construct (Location.mkloc (Longident.Lident "()") loc) None in
      with_default_loc loc @@ fun () ->
      Exp.apply
        (Exp.ident (Location.mkloc runner loc))
        [(Asttypes.Nolabel, Exp.fun_ Asttypes.Nolabel None punit e)]
    in
    (typ, runner, rewrite)

  let rec normalize_type_path env path =
    match Env.find_type path env with
    | { Types.type_manifest = Some ty; _ } -> begin
        match Ctype.expand_head env ty with
        | { Types.desc = Types.Tconstr (path, _, _) } -> path
        | _ -> path
      end
    | _ -> path

  let is_persistent_value env longident =
    let rec is_persistent_path = function
      | Path.Pident id -> Ident.persistent id
      | Path.Pdot (p, _, _) -> is_persistent_path p
      | Path.Papply (_, p) -> is_persistent_path p
    in
    try is_persistent_path (fst (Env.lookup_value longident env))
    with Not_found -> false

  let rewrite_item env async_typ pstr_item tstr_item =
    match pstr_item.Parsetree.pstr_desc, tstr_item.Typedtree.str_desc with
    | (Parsetree.Pstr_eval (e, _),
       Typedtree.Tstr_eval ({ Typedtree.exp_type = typ }, _)) ->
      begin match (Ctype.repr typ).Types.desc with
        | Types.Tconstr (path, _, _) when
            Path.same async_typ (normalize_type_path env path) ->
          let loc = pstr_item.Parsetree.pstr_loc in
          { Parsetree.pstr_desc = Parsetree.Pstr_eval (async_rewrite loc e, []);
            Parsetree.pstr_loc = loc }
        | _ -> pstr_item
      end
    | _ -> pstr_item

  let rewrite_phrase =
    let is_eval = function
      | { pstr_desc = Pstr_eval _; _ } -> true
      | _ -> false
    in
    function
    | Ptop_def pstr when List.exists is_eval pstr
                      && is_persistent_value !Toploop.toplevel_env async_runner ->
      Env.reset_cache_toplevel ();
      let snap = Btype.snapshot () in
      let pstr =
        try
          let env = !Toploop.toplevel_env in
          let path = normalize_type_path env (Env.lookup_type async_typ env) in
          let tstr, tsg, env =
            Typemod.type_structure !Toploop.toplevel_env pstr Location.none in
          List.map2 (rewrite_item env path) pstr tstr.Typedtree.str_items
        with _ ->
          pstr
      in
      Btype.backtrack snap;
      Ptop_def pstr
    | phrase -> phrase
end

let toplevel_exec_phrase ppf = function
  | { parsed = Error exn; _} -> raise exn
  | { parsed = Ok phrase; startpos; _} ->
    Warnings.reset_fatal ();
    let mapper = position_mapper {startpos with pos_fname = toplevel_fname} in
    let phrase = match phrase with
      | Ptop_def str -> Ptop_def (mapper.Ast_mapper.structure mapper str)
      | Ptop_dir _ as x -> x
    in
    let phrase = match phrase with
      | Ptop_dir _ as x -> x
      | Ptop_def s -> Ptop_def (Pparse.apply_rewriters_str ~tool_name:"expect" s)
    in
    let phrase = Async_autorun.rewrite_phrase phrase in
    if !Clflags.dump_parsetree then Printast. top_phrase ppf phrase;
    if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
    Env.reset_cache_toplevel ();
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
  let lazy () = disable_outputs in
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

let cleanup_chunk (kind, str) =
  let len = String.length str in
  if len = 0 then (kind, str) else
    let trim_from = if str.[0] = '\n' then 1 else 0 in
    let trim_to = if str.[len - 1] = '\n' then len - 1 else len in
    (kind, String.sub str trim_from (trim_to - trim_from))

let cleanup_lines lines =
  let lines = List.map cleanup_chunk lines in
  let rec join = function
    | (Chunk.Raw, str1) :: (Chunk.Raw, str2) :: rest ->
      join ((Chunk.Raw, str1 ^ "\n" ^ str2) :: rest)
    | (Chunk.OCaml, str1) :: (Chunk.OCaml, str2) :: rest ->
      join ((Chunk.OCaml, str1 ^ "\n" ^ str2) :: rest)
    | x :: xs -> x :: join xs
    | [] -> []
  in
  join lines

let dry_exec phrases =
  let rec aux acc = function
    | [] -> List.rev acc
    | (phrase, Phrase_code ()) :: rest ->
      begin match rest with
        | (_, Phrase_expect outcome) :: _ ->
          aux ((phrase, Phrase_code outcome) :: acc) rest
        | _ -> aux ((phrase, Phrase_code []) :: acc) rest
      end
    | (_, (Phrase_part _ | Phrase_expect _) as phrase) :: rest ->
      aux (phrase :: acc) rest
  in
  aux [] phrases

let eval_phrases ~fname ~dry_run fcontents =
  (* 4.03: Warnings.reset_fatal (); *)
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let exec_phrase ~capture phrase =
    let lines = ref [] in
    let capture kind =
      capture buf;
      match Buffer.contents buf with
      | "" -> ()
      | s -> Buffer.clear buf; lines := (kind, s) :: !lines
    in
    match phrase_role phrase with
    | Phrase_expect x -> Phrase_expect (cleanup_lines x)
    | Phrase_part _ as x -> x
    | Phrase_code () ->
      let out_phrase' = !Oprint.out_phrase in
      let out_phrase ppf phr = match phr with
        | Outcometree.Ophr_exception _ -> out_phrase' ppf phr
        | _ ->
          capture Chunk.Raw;
          out_phrase' ppf phr;
          capture Chunk.OCaml;
      in
      Oprint.out_phrase := out_phrase;
      let restore () = Oprint.out_phrase := out_phrase' in
      begin match toplevel_exec_phrase ppf phrase with
        | (_ : bool) -> restore ()
        | exception exn ->
          restore ();
          Location.report_exception ppf exn
      end;
      Format.pp_print_flush ppf ();
      capture Chunk.Raw;
      Phrase_code (cleanup_lines (List.rev !lines))
  in
  let parser = init_parser ~fname fcontents in
  if dry_run then  (
    let rec aux phrases = match parse_phrase parser with
      | exception End_of_file ->  List.rev phrases
      | phrase -> aux ((phrase, phrase_role phrase) :: phrases)
    in
    dry_exec (aux [])
  ) else (
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
  )
;;

let is_whitespace = function
  | ' ' | '\n' -> true
  | _ -> false

let is_all_whitespace str =
  try
    for i = 0 to String.length str - 1 do
      if not (is_whitespace str.[i]) then raise Exit
    done;
    true
  with Exit -> false

let is_ellision_line str =
  let i = ref 0 in
  let j = String.length str in
  while !i < j && is_whitespace str.[!i] do incr i done;
  !i <= j - 3 && str.[!i] = '.' && str.[!i+1] = '.' && str.[!i+2] = '.' && (
    i := !i + 3;
    while !i < j && is_whitespace str.[!i] do incr i done;
    !i = j
  )

let string_subequal subject str i =
  let len = String.length subject in
  String.length str >= len + i &&
  try
    for j = 0 to len - 1 do
      if subject.[j] <> str.[i+j] then
        raise Exit;
    done;
    true
  with Exit ->
    false

let match_outcome_chunk (k1,outcome) (k2,expected) =
  k1 = k2 &&
  let rec split_chunks acc str i0 i =
    match String.index_from str i '\n' with
    | exception Not_found ->
      let acc =
        if is_ellision_line (String.sub str i (String.length str - i))
        then "" :: String.sub str i0 i :: acc
        else String.sub str i0 (String.length str - i0) :: acc
      in
      List.rev acc
    | j ->
      if is_ellision_line (String.sub str i (j - i)) then
        split_chunks (String.sub str i0 (i - i0) :: acc) str (j + 1) (j + 1)
      else
        split_chunks acc str i0 (j + 1)
  in
  match split_chunks [] expected 0 0 with
  | [] -> assert false
  | x :: xs ->
    string_subequal x outcome 0 &&
    let rec match_chunks i = function
      | [] -> i = String.length outcome
      | [x] ->
        let i' = String.length outcome - String.length x in
        i' >= i && string_subequal x outcome i'
      | x :: xs ->
        let bound = String.length outcome - String.length x in
        let i = ref i in
        while !i <= bound && not (string_subequal x outcome !i)
        do incr i done;
        if !i > bound then false
        else match_chunks (!i + String.length x) xs
    in
    match_chunks (String.length x) xs

let match_outcome xs ys =
  List.length xs = List.length ys &&
  List.for_all2 match_outcome_chunk xs ys

let rec valid_phrases = function
  | [] -> true
  | (_, Phrase_part _) :: rest -> valid_phrases rest
  | (_, Phrase_code outcome) :: (_, Phrase_expect outcome') :: rest ->
    match_outcome outcome outcome' && valid_phrases rest
  | (_, Phrase_code outcome) :: rest ->
    List.for_all (fun (_,s) -> is_all_whitespace s) outcome && valid_phrases rest
  | (_, Phrase_expect _) :: _ -> false

(* Skip spaces as well as ';;' *)
let skip_whitespace contents ?(stop=String.length contents) start =
  let rec loop start =
    if start >= stop then start else
      match contents.[start] with
      | ' ' | '\t' | '\n' -> loop (start + 1)
      | ';' when start + 1 < stop && contents.[start+1] = ';' ->
        loop (start + 2)
      | _ -> start
  in
  loop start

let phrase_code contents phrase =
  let stop = phrase.endpos.pos_cnum in
  let start = skip_whitespace contents ~stop phrase.startpos.pos_cnum in
  String.sub contents start (stop - start)

let phrase_whitespace contents phrase rest =
  let start = phrase.endpos.pos_cnum in
  let stop = match rest with
    | [] -> String.length contents
    | (phrase', _) :: _ ->
      skip_whitespace contents phrase'.startpos.pos_cnum
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
      if List.for_all (fun (_,s) -> is_all_whitespace s) expect_code then
        Printf.fprintf oc "%s%s" phrase_code expect_ws
      else (
        let string_of_kind = function
          | Chunk.Raw -> ""
          | Chunk.OCaml -> "ocaml "
        in
        let output_expect oc = function
          | [] -> ()
          | [(kind, str)] when not (String.contains str '\n') ->
            let k = string_of_kind kind in
            Printf.fprintf oc "%s%s{|%s|}" (if k <> "" then " " else "") k str
          | xs ->
            let rec aux first = function
              | [] -> ()
              | (k,s) :: xs ->
                let k = string_of_kind k in
                let pre = if first then (if k <> "" then " " else "") else "\n" in
                let post = if xs = [] then "" else ";" in
                if not (String.contains s '\n')
                then Printf.fprintf oc "%s%s{|%s|}%s" pre k s post
                else Printf.fprintf oc "%s%s{|\n%s\n|}%s" pre k s post;
                aux false xs
            in
            aux true xs
        in
        Printf.fprintf oc "%s%s[%%%%expect%a];;%s"
          phrase_code phrase_ws output_expect expect_code expect_ws;
      );
      aux rest
    | (_, Phrase_expect _) :: rest ->
      aux rest
  in aux

let document_of_phrases contents matched phrases =
  let rec parts_of_phrase part acc = function
    | (_, Phrase_part part') :: rest ->
      { Part. name = part; chunks = List.rev acc } ::
      parts_of_phrase part' [] rest
    | (_, Phrase_expect _) :: rest ->
      parts_of_phrase part acc rest
    | (phrase, Phrase_code toplevel_responses) :: rest ->
      let ocaml_code = phrase_code contents phrase in
      let chunk = {Chunk. ocaml_code; toplevel_responses} in
      parts_of_phrase part (chunk :: acc) rest
    | [] ->
      if part <> "" || acc <> [] then
        [{ Part. name = part; chunks = List.rev acc }]
      else
        []
  in
  let parts = parts_of_phrase "" [] phrases in
  { Document. matched; parts }

let process_expect_file ~fname ~dry_run ~use_color ~in_place ~sexp_output =
  let file_contents =
    let ic = open_in fname in
    let len = in_channel_length ic in
    let result = really_input_string ic len in
    close_in_noerr ic;
    result
  in
  let phrases = eval_phrases ~fname ~dry_run file_contents in
  let success = valid_phrases phrases in
  let oname = if in_place then fname else fname ^ ".corrected" in
  if success && not in_place && Sys.file_exists oname then
    Sys.remove oname;
  let phrases =
    if success then (
      (* If successful, replace output by expectation to preserve ellisions. *)
      dry_exec (List.map (fun (p, _) -> p, phrase_role p) phrases)
    ) else (
      (* Otherwise, generate corrected file and keep toplevel output. *)
      let oc = open_out_bin (oname ^ ".tmp") in
      output_phrases oc file_contents phrases;
      flush oc;
      close_out oc;
      Sys.rename (oname ^ ".tmp") oname;
      phrases
    )
  in
  if sexp_output then (
    document_of_phrases file_contents success phrases
    |> Document.sexp_of_t
    |> Sexplib.Sexp.output stdout_backup
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
let dry_run     = ref false

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
    process_expect_file ~fname
      ~dry_run:!dry_run ~use_color:!use_color
      ~in_place:!in_place ~sexp_output:!sexp_output
  in
  exit (if success then 0 else 1)
;;

let args =
  Arg.align
    [ "-in-place", Arg.Set in_place,    " Overwrite file in place"
    ; "-sexp"    , Arg.Set sexp_output, " Output the result as a s-expression instead of diffing"
    ; "-verbose" , Arg.Set verbose, " Include outcome of phrase evaluation (like ocaml toplevel)"
    ; "-dry-run" , Arg.Set dry_run, " Don't execute code, only return expected outcome"
    ]

let print_version () =
  Printf.fprintf stdout_backup "topexect, version %s\n" Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  Printf.fprintf stdout_backup "%s\n" Sys.ocaml_version;
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

(* BLACK MAGIC: patch field of a module at runtime *)
let monkey_patch (type a) (type b) (m: a) (prj: unit -> b) (v : b) =
  let m = Obj.repr m in
  let v = Obj.repr v in
  let v' = Obj.repr (prj ()) in
  if v' == v then () else (
    try
      for i = 0 to Obj.size m - 1 do
        if Obj.field m i == v' then (
          Obj.set_field m i v;
          if Obj.repr (prj ()) == v then raise Exit;
          Obj.set_field m i v';
        )
      done;
      invalid_arg "monkey_patch: field not found"
    with Exit -> ()
  )

let main () =
  let module M = struct
    module type T = module type of Env
    let field () = Env.without_cmis
    let replacement f x = f x
    let () = monkey_patch (module Env : T) field replacement
  end in
  let usage =
    Printf.sprintf "Usage: %s [OPTIONS] FILE [ARGS]\n"
      (Filename.basename Sys.argv.(0))
  in
  try
    let args = Arg.align (args @ Options.list) in
    Arg.parse args process_file (usage ^ "\nOptions are:");
    Printf.fprintf stderr_backup "%s\n%!" usage;
    exit 2
  with exn ->
    ignore (Format.flush_str_formatter ());
    Location.report_exception Format.str_formatter exn;
    Printf.fprintf stderr_backup "%s\n%!" (Format.flush_str_formatter ());
    exit 2
;;
let () = main ();;

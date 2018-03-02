module Linked = struct
  include (Topdirs : sig end)
  include (Ephemeron : sig end)
  include (Uchar : sig end)
  include (Condition : sig end)
end

open Lexing
open Parsetree
open Sexplib.Conv
open Ocaml_topexpect

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

let parse_phrase (_contents, lexbuf) =
  let startpos = lexbuf.Lexing.lex_curr_p in
  let parsed = match Parse.toplevel_phrase lexbuf with
    | phrase -> Ok phrase
    | exception exn ->
      let exn = match Location.error_of_exn exn with
        | None -> raise exn
        | Some `Already_displayed -> raise exn
        | Some (`Ok error) -> Location.Error (shift_location_error startpos error)
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
  | Phrase_expect of { location: Location.t; responses: Chunk.response list; nondeterministic: bool }
  | Phrase_part of { location: Location.t; name: string }

exception Cannot_parse_payload of Location.t

let string_of_location {Location.loc_start = {pos_fname; pos_lnum; pos_bol; pos_cnum};_} =
  Printf.sprintf "%s, line %d, col %d" pos_fname pos_lnum (pos_cnum - pos_bol)

let payload_constants loc = function
  | PStr [{pstr_desc = Pstr_eval (expr, _); _}] ->
    let one {pexp_loc; pexp_desc; _} = match pexp_desc with
      | Pexp_apply ({pexp_desc = Pexp_ident ident; _},
                    [Asttypes.Nolabel, {pexp_desc = Pexp_constant const; _}]) ->
        (pexp_loc, Some ident, const)
      | Pexp_constant const -> (pexp_loc, None, const)
      | _ -> raise (Cannot_parse_payload pexp_loc)
    in
    let rec consts = function
      | {pexp_desc=Pexp_sequence(e, rest); _} -> one e :: consts rest
      | e -> [one e]
    in
    consts expr
  | PStr [] -> []
  | _ -> raise (Cannot_parse_payload loc)

let payload_strings loc = function
  | PStr [] -> []
  | x ->
    let aux = function
      | _, Some {Location.txt = Longident.Lident "ocaml"; _},
        Pconst_string (str, _) -> (Chunk.OCaml, str)
      | _, None, Pconst_string (str, _) -> (Chunk.Raw, str)
      | loc, _, _ -> raise (Cannot_parse_payload loc)
    in
    List.map aux (payload_constants loc x)

let constant_payload const = PStr [Ast_helper.(Str.eval (Exp.constant const))]
let string_payload x = constant_payload (Pconst_string (x, None))

let attr_is x name = x.Asttypes.txt = name

let phrase_role phrase = match phrase.parsed with
  | Ok (Ptop_def [{pstr_desc = Pstr_extension((attr, payload), _attrs); pstr_loc}])
    when List.exists (attr_is attr) ["expect"; "expect.nondeterministic"] ->
    begin match payload_strings pstr_loc payload with
      | responses ->
        let nondeterministic = attr_is attr "expect.nondeterministic" in
        Phrase_expect { location = pstr_loc; responses; nondeterministic }
      | exception (Cannot_parse_payload loc) ->
        prerr_endline (string_of_location loc ^ ": cannot parse [%%expect] payload");
        Phrase_code ()
   end
  | Ok (Ptop_def [{pstr_desc = Pstr_attribute (name, payload); pstr_loc}])
    when name.Asttypes.txt = "part" ->
    begin match payload_strings pstr_loc payload with
      | [Chunk.Raw, part] -> Phrase_part { location = pstr_loc; name = part }
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
let silent = ref false
let () = Hashtbl.add Toploop.directive_table "silent"
    (Toploop.Directive_bool (fun x -> silent := x))
let verbose_findlib = ref false

let is_findlib_directive =
  let findlib_directive = function
    | "require" | "use" | "camlp4o" | "camlp4r" | "thread" -> true
    | _ -> false
  in
  function
  | { parsed = Ok (Ptop_dir (dir, _)); _ } -> findlib_directive dir
  | _ -> false

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

  let normalize_type_path env path =
    match Env.find_type path env with
    | { Types.type_manifest = Some ty; _ } -> begin
        match Ctype.expand_head env ty with
        | { Types.desc = Types.Tconstr (path, _, _);_ } -> path
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
       Typedtree.Tstr_eval ({ Typedtree.exp_type = typ; _ }, _)) ->
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
          let tstr, _tsg, env =
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
        | (_, Phrase_expect { responses; _ }) :: _ ->
          aux ((phrase, Phrase_code responses) :: acc) rest
        | _ -> aux ((phrase, Phrase_code []) :: acc) rest
      end
    | (_, (Phrase_part _ | Phrase_expect _) as phrase) :: rest ->
      aux (phrase :: acc) rest
  in
  aux [] phrases

let eval_phrases ~run_nondeterministic ~fname ~dry_run fcontents =
  (* 4.03: Warnings.reset_fatal (); *)
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let exec_code ~capture phrase =
    let lines = ref [] in
    let capture kind =
      capture buf;
      match Buffer.contents buf with
      | "" -> ()
      | s -> Buffer.clear buf; lines := (kind, s) :: !lines
    in
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
    if !silent || (not !verbose_findlib && is_findlib_directive phrase) then
      Phrase_code []
    else
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
            let rec process_phrase chunks phrase =
              match phrase_role phrase with
              | Phrase_expect x ->
                next_phrase ((phrase, Phrase_expect {x with responses = cleanup_lines x.responses}) :: chunks)
              | Phrase_part _ as x ->
                next_phrase ((phrase, x) :: chunks)
              | Phrase_code () ->
                match parse_phrase parser with
                | exception End_of_file ->
                  List.rev ((phrase, exec_code ~capture phrase) :: chunks)
                | phrase' ->
                  let role = match phrase_role phrase' with
                    | Phrase_expect { nondeterministic = true; responses; _ }
                      when not run_nondeterministic -> Phrase_code responses
                    | _ -> exec_code ~capture phrase
                  in
                  process_phrase ((phrase, role) :: chunks) phrase'
            and next_phrase chunks = match parse_phrase parser with
              | exception End_of_file -> List.rev chunks
              | phrase -> process_phrase chunks phrase
            in
            next_phrase []
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

(* Check if output matches expectations and keep ellisions when possible *)
let validate_phrases run_nondeterministic =
  let rec aux success acc = function
    | [] -> (success, List.rev acc)
    | (_, Phrase_part _ as entry) :: rest ->
      aux success (entry :: acc) rest
    | (p0, Phrase_code outcome) :: (p1, Phrase_expect outcome') :: rest ->
      let success' =
        if outcome'.nondeterministic && not run_nondeterministic then
          true
        else
          match_outcome outcome outcome'.responses
      in
      let acc =
        if success' then
          (p1, Phrase_expect outcome') :: (p0, Phrase_code outcome'.responses) :: acc
        else
          (p1, Phrase_expect outcome') :: (p0, Phrase_code outcome) :: acc
      in
      aux (success && success') acc rest
    | (_, Phrase_code outcome as x) :: rest ->
      let success =
        success && List.for_all (fun (_,s) -> is_all_whitespace s) outcome
      in
      aux success (x :: acc) rest
    | (_, Phrase_expect _ as x) :: rest ->
      aux false (x :: acc) rest
  in
  fun phrases -> aux true [] phrases


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

let phrase_contents contents ?start ?stop phrase =
  let stop = match stop with
    | None -> phrase.endpos.pos_cnum
    | Some stop -> stop
  in
  let start = match start with
    | None -> phrase.startpos.pos_cnum
    | Some start -> start
  in
  let start = skip_whitespace contents ~stop start in
  String.sub contents start (stop - start)

let phrase_whitespace contents phrase rest =
  let start = phrase.endpos.pos_cnum in
  let stop = match rest with
    | [] -> String.length contents
    | (phrase', _) :: _ ->
      skip_whitespace contents phrase'.startpos.pos_cnum
  in
  String.sub contents start (stop - start)

let find_delim s =
  let len = String.length s in
  let delims = ref [] in
  let beginning = ref (-1) in
  for i = 0 to len - 1 do
    match s.[i] with
    | '{' -> beginning := i
    | '|' when !beginning = -1 || s.[!beginning] <> '{' -> beginning := i
    | ('|' | '}' as c) when !beginning <> -1 && (c = '|' || s.[!beginning] = '|') ->
      let delim = String.sub s (!beginning + 1) (i - !beginning - 1) in
      begin match !delims with
        | delim' :: _ when delim' = delim -> ()
        | delims' -> delims := delim :: delims'
      end;
      beginning := -1
    | 'a'..'z' | '_' -> ()
    | _ -> beginning := -1
  done;
  let delims = !delims in
  let candidates = [""; "escape"; "x"; "y"; "z"] in
  match List.find (fun delim -> not (List.mem delim delims)) candidates with
  | candidate -> candidate
  | exception Not_found ->
    (* Generate a string that is not in the list of delimiters *)
    let next b =
      try
        for i = Bytes.length b - 1 downto 0 do
          match Bytes.get b i with
          | 'z' -> Bytes.set b i 'a'
          | c ->
            Bytes.set b i (Char.chr (Char.code c + 1));
            raise Exit
        done;
        Bytes.cat b (Bytes.unsafe_of_string "a")
      with Exit -> b
    in
    let rec exhaust b =
      if not (List.mem (Bytes.unsafe_to_string b) delims)
      then Bytes.unsafe_to_string b
      else exhaust (next b)
    in
    exhaust (Bytes.of_string "")

let output_phrases oc contents =
  let rec aux = function
    | [] -> ()
    | (phrase, Phrase_part {name; location}) :: rest ->
      Printf.fprintf oc "%s[@@@part %S];;\n"
        (phrase_contents contents phrase ~stop:location.loc_start.pos_cnum) name;
      aux rest
    | (phrase, Phrase_code expect_code) :: rest ->
      let phrase_post, expect_pre, expect_post, nondeterministic, rest =
        match rest with
        | (phrase_expect, Phrase_expect x) :: rest' ->
          (phrase_whitespace contents phrase rest,
           phrase_contents contents phrase_expect ~stop:x.location.loc_start.pos_cnum,
           phrase_whitespace contents phrase_expect rest',
           x.nondeterministic,
           rest')
        | _ ->
          ("\n", "", phrase_whitespace contents phrase rest, false, rest)
      in
      let phrase_code = phrase_contents contents phrase in
      if List.for_all (fun (_,s) -> is_all_whitespace s) expect_code &&
         not nondeterministic then
        Printf.fprintf oc "%s%s%s" phrase_code expect_pre expect_post
      else (
        let string_of_kind = function
          | Chunk.Raw -> ""
          | Chunk.OCaml -> "ocaml "
        in
        let output_expect oc = function
          | [] -> ()
          | [(kind, str)] when not (String.contains str '\n') ->
            let k = string_of_kind kind in
            let delim = find_delim str in
            Printf.fprintf oc "%s%s{%s|%s|%s}" (if k <> "" then " " else "") k delim str delim
          | xs ->
            let rec aux first = function
              | [] -> ()
              | (k,s) :: xs ->
                let k = string_of_kind k in
                let pre = if first then (if k <> "" then " " else "") else "\n" in
                let post = if xs = [] then "" else ";" in
                let delim = find_delim s in
                if not (String.contains s '\n') then
                  Printf.fprintf oc "%s%s{%s|%s|%s}%s" pre k delim s delim post
                else
                  Printf.fprintf oc "%s%s{%s|\n%s\n|%s}%s" pre k delim s delim post;
                aux false xs
            in
            aux true xs
        in
        Printf.fprintf oc "%s%s%s[%%%%expect%s%a];;%s"
          phrase_code phrase_post
          expect_pre
          (if nondeterministic then ".nondeterministic" else "")
          output_expect expect_code expect_post;
      );
      aux rest
    | (phrase, Phrase_expect {location; _}) :: rest ->
      Printf.fprintf oc "%s" (phrase_contents contents phrase ~stop:location.loc_start.pos_cnum);
      aux rest
  in aux

let document_of_phrases contents matched phrases =
  let rec parts_of_phrase part acc = function
    | (_, Phrase_part { name; _ }) :: rest ->
      Part.v ~name:part ~chunks:(List.rev acc) ::
      parts_of_phrase name [] rest
    | (_, Phrase_expect _) :: rest ->
      parts_of_phrase part acc rest
    | (phrase, Phrase_code toplevel_responses) :: rest ->
      let ocaml_code = phrase_contents contents phrase in
      let chunk = Chunk.v ~ocaml_code ~toplevel_responses in
      parts_of_phrase part (chunk :: acc) rest
    | [] ->
      if part <> "" || acc <> [] then
        [Part.v ~name:part ~chunks:(List.rev acc)]
      else
        []
  in
  let parts = parts_of_phrase "" [] phrases in
  Document.v ~matched ~parts

let process_expect_file ~run_nondeterministic ~fname ~dry_run ~use_color:_ ~in_place ~sexp_output =
  let file_contents =
    let ic = open_in fname in
    let len = in_channel_length ic in
    let result = really_input_string ic len in
    close_in_noerr ic;
    result
  in
  let phrases = eval_phrases ~run_nondeterministic ~fname ~dry_run file_contents in
  let success, phrases = validate_phrases run_nondeterministic phrases in
  let oname = if in_place then fname else fname ^ ".corrected" in
  if success && not in_place && Sys.file_exists oname then
    Sys.remove oname;
  let phrases =
    if success then phrases
    else (
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
    |> Document.rwo
    |> Document.sexp_of_rwo
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
let run_nondeterministic = ref false

let process_file fname =
  let cmd_line =
    Array.sub Sys.argv !Arg.current (Array.length Sys.argv - !Arg.current)
  in
  override_sys_argv cmd_line;
  Toploop.set_paths ();
  Compmisc.init_path true;
  Toploop.toplevel_env := Compmisc.initial_env ();
  Sys.interactive := false;
  let _success =
    process_expect_file ~fname
      ~run_nondeterministic:!run_nondeterministic
      ~dry_run:!dry_run ~use_color:!use_color
      ~in_place:!in_place ~sexp_output:!sexp_output
  in
  exit 0
;;

let args =
  Arg.align
    [ "-in-place", Arg.Set in_place,    " Overwrite file in place"
    ; "-sexp"    , Arg.Set sexp_output, " Output the result as a s-expression instead of diffing"
    ; "-verbose" , Arg.Set verbose, " Include outcome of phrase evaluation (like ocaml toplevel)"
    ; "-dry-run" , Arg.Set dry_run, " Don't execute code, only return expected outcome"
    ; "-run-nondeterministic" , Arg.Set run_nondeterministic, " Run non-deterministic tests"
    ; "-verbose-findlib", Arg.Set verbose_findlib, " Include outcome of findlib directives (#require, #use, ...)"
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
    (* let _dtimings = set print_timings *)
    let _dinstr = set dump_instr

    let anonymous s = process_file s
    let _args _ = failwith "Arg.read_arg not implemented"
    let _args0 _ = failwith "Arg.read_arg0 not implemented"
    (*let _args = Arg.read_arg
    let _args0 = Arg.read_arg0*)
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
  Topfind.don't_load_deeply ["unix"; "findlib.top"; "findlib.internal"; "compiler-libs.toplevel"; "ppx_sexp_conv"];

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

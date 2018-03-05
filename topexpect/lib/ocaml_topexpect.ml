open Sexplib.Conv

module Chunk = struct
  type kind = OCaml | Raw
    [@@deriving sexp]

  type response = (kind * string)
    [@@deriving sexp]

  type t =
    { ocaml_code : string;
      toplevel_responses : response list; }
    [@@deriving sexp]

  let v ~ocaml_code ~toplevel_responses = {ocaml_code; toplevel_responses}
  let code c = c.ocaml_code
  let warnings (_ : t) : string =  ""
  let responses c = c.toplevel_responses
  let stdout (_ : t) = ""
  let evaluated (_ : t) = true
end

module Part = struct
  type t =
    { name : string;
      chunks : Chunk.t list; }
    [@@deriving sexp]

  let v ~name ~chunks = { name; chunks }
  let name {name;_} = name
  let chunks {chunks;_} = chunks
end

module Document = struct
  type t =
    { parts : Part.t list; matched : bool; }
    [@@deriving sexp]

  let v ~parts ~matched = {parts; matched}
  let parts {parts;_} = parts
  let matched {matched;_} = matched
end

module Lexbuf = struct

  open Lexing

  type t = {
    contents: string;
    lexbuf  : lexbuf;
  }

  let toplevel_fname = "//toplevel//"

  let shift_toplevel_position ~start pos = {
    pos_fname = toplevel_fname;
    pos_lnum = pos.pos_lnum - start.pos_lnum + 1;
    pos_bol  = pos.pos_bol  - start.pos_cnum - 1;
    pos_cnum = pos.pos_cnum - start.pos_cnum - 1;
  }

  let shift_toplevel_location ~start loc =
    let open Location in
    {loc with loc_start = shift_toplevel_position ~start loc.loc_start;
              loc_end = shift_toplevel_position ~start loc.loc_end}

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

  let v ~fname contents =
    let lexbuf = Lexing.from_string contents in
    lexbuf.lex_curr_p <- {initial_pos with pos_fname = fname};
    Location.input_name := fname;
    { contents; lexbuf }

  let of_file fname =
    let ic = open_in fname in
    let len = in_channel_length ic in
    let result = really_input_string ic len in
    close_in_noerr ic;
    v ~fname result

  let shift_location_error start =
    let open Location in
    let rec aux (error : Location.error) =
      {error with sub = List.map aux error.sub;
                  loc = shift_toplevel_location ~start error.loc}
    in
    aux

  let position_mapper start =
    let open Ast_mapper in
    let start = {start with pos_fname = toplevel_fname} in
    let location mapper loc =
      shift_toplevel_location ~start (default_mapper.location mapper loc)
    in
    {default_mapper with location}

end

module Phrase = struct

  open Lexing
  open Parsetree

  (** {1 Phrase parsing} *)

  type t = {
    startpos : position;
    endpos   : position;
    parsed   : (toplevel_phrase, exn) result;
  }

  let result t = t.parsed
  let start t = t.startpos

  let read lexbuf =
    let startpos = lexbuf.Lexing.lex_curr_p in
    let parsed = match Parse.toplevel_phrase lexbuf with
      | phrase -> Ok phrase
      | exception exn ->
        let exn = match Location.error_of_exn exn with
          | None -> raise exn
          | Some `Already_displayed -> raise exn
          | Some (`Ok error) ->
            Location.Error (Lexbuf.shift_location_error startpos error)
        in
        if lexbuf.Lexing.lex_last_action <> Lexbuf.semisemi_action then begin
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

  let read doc = match read doc.Lexbuf.lexbuf with
    | exception End_of_file -> None
    | t -> Some t

  (** *)

  type 'a kind =
    | Code of 'a
    | Expect of { location: Location.t;
                  responses: Chunk.response list;
                  nondeterministic: bool }
    | Part of { location: Location.t; name: string }

  type v = (t * Chunk.response list kind) list

  exception Cannot_parse_payload of Location.t

  let string_of_location
      {Location.loc_start = {pos_fname; pos_lnum; pos_bol; pos_cnum};_}
    =
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

  let attr_is x name = x.Asttypes.txt = name

  let kind phrase = match phrase.parsed with
    | Ok (Ptop_def [{pstr_desc = Pstr_extension((attr, payload), _attrs); pstr_loc}])
      when List.exists (attr_is attr) ["expect"; "expect.nondeterministic"] ->
      begin match payload_strings pstr_loc payload with
        | responses ->
          let nondeterministic = attr_is attr "expect.nondeterministic" in
          Expect { location = pstr_loc; responses; nondeterministic }
        | exception (Cannot_parse_payload loc) ->
          prerr_endline (string_of_location loc ^ ": cannot parse [%%expect] payload");
          Code ()
      end
    | Ok (Ptop_def [{pstr_desc = Pstr_attribute (name, payload); pstr_loc}])
      when name.Asttypes.txt = "part" ->
      begin match payload_strings pstr_loc payload with
        | [Chunk.Raw, part] -> Part { location = pstr_loc; name = part }
        | _ ->
          prerr_endline (string_of_location pstr_loc ^ ": cannot parse [@@@part] payload");
          Code ()
        | exception (Cannot_parse_payload loc) ->
          prerr_endline
            (string_of_location loc ^ ": cannot parse [@@@part] payload");
          Code ()
      end
    | _ -> Code ()

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

  let contents doc ?start ?stop phrase =
    let stop = match stop with
      | None -> phrase.endpos.pos_cnum
      | Some stop -> stop
    in
    let start = match start with
      | None -> phrase.startpos.pos_cnum
      | Some start -> start
    in
    let start = skip_whitespace doc.Lexbuf.contents ~stop start in
    String.sub doc.contents start (stop - start)

  let whitespace doc phrase rest =
    let start = phrase.endpos.pos_cnum in
    let stop = match rest with
      | [] -> String.length doc.Lexbuf.contents
      | (p, _) :: _ -> skip_whitespace doc.contents p.startpos.pos_cnum
    in
    String.sub doc.contents start (stop - start)

  let document doc ~matched phrases =
    let rec parts_of_phrase part acc = function
      | (_, Part { name; _ }) :: rest ->
        Part.v ~name:part ~chunks:(List.rev acc) ::
        parts_of_phrase name [] rest
      | (_, Expect _) :: rest ->
        parts_of_phrase part acc rest
      | (phrase, Code toplevel_responses) :: rest ->
        let ocaml_code = contents doc phrase in
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

  let is_findlib_directive =
    let findlib_directive = function
      | "require" | "use" | "camlp4o" | "camlp4r" | "thread" -> true
      | _ -> false
    in
    function
    | { parsed = Ok (Ptop_dir (dir, _)); _ } -> findlib_directive dir
    | _ -> false

  let dry_exec phrases =
    let rec aux acc = function
      | [] -> List.rev acc
      | (phrase, Code ()) :: rest ->
        begin match rest with
          | (_, Expect { responses; _ }) :: _ ->
            aux ((phrase, Code responses) :: acc) rest
          | _ -> aux ((phrase, Code []) :: acc) rest
        end
      | (_, (Part _ | Expect _) as phrase) :: rest ->
        aux (phrase :: acc) rest
    in
    aux [] phrases

  let read_all doc =
    let rec aux phrases = match read doc with
      | None        ->  List.rev phrases
      | Some phrase -> aux ((phrase, kind phrase) :: phrases)
    in
    dry_exec (aux [])

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

(* Check if output matches expectations and keep ellisions when
   possible *)
let validate ~run_nondeterministic =
  let rec aux success acc = function
    | [] -> (success, List.rev acc)
    | (_, Part _ as entry) :: rest ->
      aux success (entry :: acc) rest
    | (p0, Code outcome) :: (p1, Expect outcome') :: rest ->
      let success' =
        if outcome'.nondeterministic && not run_nondeterministic then
          true
        else
          match_outcome outcome outcome'.responses
      in
      let acc =
        if success' then
          (p1, Expect outcome') :: (p0, Code outcome'.responses) :: acc
        else
          (p1, Expect outcome') :: (p0, Code outcome) :: acc
      in
      aux (success && success') acc rest
    | (_, Code outcome as x) :: rest ->
      let success =
        success && List.for_all (fun (_,s) -> is_all_whitespace s) outcome
      in
      aux success (x :: acc) rest
    | (_, Expect _ as x) :: rest ->
      aux false (x :: acc) rest
  in
  fun phrases -> aux true [] phrases

let output oc lexbuf =
  let rec aux = function
    | [] -> ()
    | (phrase, Part {name; location}) :: rest ->
      Printf.fprintf oc "%s[@@@part %S];;\n"
        (contents lexbuf phrase ~stop:location.loc_start.pos_cnum) name;
      aux rest
    | (phrase, Code expect_code) :: rest ->
      let phrase_post, expect_pre, expect_post, nondeterministic, rest =
        match rest with
        | (phrase_expect, Expect x) :: rest' ->
          (whitespace lexbuf phrase rest,
           contents lexbuf phrase_expect ~stop:x.location.loc_start.pos_cnum,
           whitespace lexbuf phrase_expect rest',
           x.nondeterministic,
           rest')
        | _ ->
          ("\n", "", whitespace lexbuf phrase rest, false, rest)
      in
      let phrase_code = contents lexbuf phrase in
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
    | (phrase, Expect {location; _}) :: rest ->
      Printf.fprintf oc "%s"
        (contents lexbuf phrase ~stop:location.loc_start.pos_cnum);
      aux rest
  in aux

end

module Code_block = struct
  type metadata = {
    language_tag : string Odoc_parser.Loc.with_location;
    labels : string Odoc_parser.Loc.with_location option;
  }

  type t = {
    location : Odoc_parser.Loc.span;
    metadata : metadata option;
    contents : string;
  }
end

let drop_last lst =
  match List.rev lst with
  | [] -> None
  | last :: rev_tl -> Some (List.rev rev_tl, last)

(* drop_first_and_last [1; 2; 3; 4] = Some (1, Some ([2; 3], 4)). *)
let drop_first_and_last = function
  | [] -> None
  | first :: tl -> Some (first, drop_last tl)

let slice lines ~(start : Odoc_parser.Loc.point) ~(end_ : Odoc_parser.Loc.point)
    =
  let lines_to_include =
    Util.Array.slice lines ~from:(start.line - 1) ~to_:(end_.line - 1)
    |> Array.to_list
  in
  match drop_first_and_last lines_to_include with
  | None -> ""
  | Some (line, None) ->
      String.sub line start.column (end_.column - start.column)
  (* Imagine we were slicing the file from (Line 2, Column 3) to (Line 6, Column 7):

       0123456789
       1 ----------
       2 ---[---
       3 ---------
       4 --
       5 ----------
       6 -------]--
       7 ----------
       8 ----------

       The case below handles this multiline case, concatenating the included substrings
       from lines 2-6 ([lines_to_include]). *)
  | Some (first_line, Some (stripped, last_line)) ->
      let first_line =
        String.sub first_line start.column
          (String.length first_line - start.column)
      in
      let last_line = String.sub last_line 0 end_.column in
      String.concat "\n" ([ first_line ] @ stripped @ [ last_line ])

let extract_code_blocks ~(location : Lexing.position) ~docstring =
  let rec acc blocks =
    List.map
      (fun block ->
        match Odoc_parser.Loc.value block with
        | `Code_block (metadata, { Odoc_parser.Loc.value = contents; _ }) ->
            let metadata =
              Option.map
                (fun (language_tag, labels) ->
                  Code_block.{ language_tag; labels })
                metadata
            in
            [ { Code_block.location = block.location; metadata; contents } ]
        | `List (_, _, lists) -> List.map acc lists |> List.concat
        | _ -> [])
      blocks
    |> List.concat
  in
  let parsed = Odoc_parser.parse_comment ~location ~text:docstring in
  List.iter
    (fun error -> failwith (Odoc_parser.Warning.to_string error))
    (Odoc_parser.warnings parsed);
  List.map
    (fun element ->
      match element with
      | { Odoc_parser.Loc.value = #Odoc_parser.Ast.nestable_block_element; _ }
        as e ->
          acc [ e ]
      | { value = `Tag tag; _ } -> (
          match tag with
          | `Deprecated blocks -> acc blocks
          | `Param (_, blocks) -> acc blocks
          | `Raise (_, blocks) -> acc blocks
          | `Return blocks -> acc blocks
          | `See (_, _, blocks) -> acc blocks
          | `Before (_, blocks) -> acc blocks
          | _ -> [])
      | { value = `Heading _; _ } -> [])
    (Odoc_parser.ast parsed)
  |> List.concat

let docstrings lexbuf =
  let rec loop list =
    match Lexer.token_with_comments lexbuf with
    | Parser.EOF -> list
    | Parser.DOCSTRING docstring ->
        let docstring =
          ( Docstrings.docstring_body docstring,
            Docstrings.docstring_loc docstring )
        in
        loop (docstring :: list)
    | _ -> loop list
  in
  loop [] |> List.rev

let convert_pos (p : Lexing.position) (pt : Odoc_parser.Loc.point) =
  { p with pos_lnum = pt.line; pos_cnum = pt.column }

let convert_loc (loc : Location.t) (sp : Odoc_parser.Loc.span) =
  let loc_start = convert_pos loc.loc_start sp.start in
  let loc_end = convert_pos loc.loc_end sp.end_ in
  { loc with loc_start; loc_end }

let docstring_code_blocks str =
  Lexer.handle_docstrings := true;
  Lexer.init ();
  List.map
    (fun (docstring, (cmt_loc : Location.t)) ->
      let location =
        { cmt_loc.loc_start with pos_cnum = cmt_loc.loc_start.pos_cnum + 3 }
      in
      let blocks = extract_code_blocks ~location ~docstring in
      List.map
        (fun (b : Code_block.t) -> (b, convert_loc cmt_loc b.location))
        blocks)
    (docstrings (Lexing.from_string str))
  |> List.concat

let make_block ~loc code_block =
  let handle_header = function
    | Some Code_block.{ language_tag; labels } -> (
        let header =
          Block.Header.of_string (Odoc_parser.Loc.value language_tag)
        in
        match labels with
        | None -> Ok (header, [])
        | Some labels -> (
            let labels = Odoc_parser.Loc.value labels |> String.trim in
            match Label.of_string labels with
            | Ok labels -> Ok (header, labels)
            | Error msgs -> Error (List.hd msgs)
            (* TODO: Report precise location *)))
    | None ->
        (* If not specified, blocks are run as ocaml blocks *)
        Ok (Some OCaml, [])
  in
  match handle_header code_block.Code_block.metadata with
  | Error _ as e -> e
  | Ok (header, labels) ->
      let contents = String.split_on_char '\n' code_block.contents in
      Block.mk ~loc ~section:None ~labels ~header ~contents ~legacy_labels:false
        ~errors:[]

let code_block_markup code_block =
  let open Document in
  let opening =
    match code_block.Code_block.metadata with
    | Some { language_tag; labels } ->
        let labels =
          match labels with
          | Some s -> [ Text " "; Text (Odoc_parser.Loc.value s) ]
          | None -> []
        in
        [ Text "{@"; Text (Odoc_parser.Loc.value language_tag) ]
        @ labels @ [ Text "[" ]
    | None -> [ Text "{[" ]
  in
  let hpad =
    let has_several_lines = String.contains code_block.contents '\n' in
    let column = code_block.location.start.column in
    if not has_several_lines then ""
    else Astring.String.v ~len:column (fun _ -> ' ')
  in
  (opening, [ Text (hpad ^ "]}") ])

let parse_mli file_contents =
  (* Find the locations of the code blocks within [file_contents], then slice it up into
     [Text] and [Block] parts by using the starts and ends of those blocks as
     boundaries. *)
  let code_blocks = docstring_code_blocks file_contents in
  let cursor = ref { Odoc_parser.Loc.line = 1; column = 0 } in
  let lines = String.split_on_char '\n' file_contents |> Array.of_list in
  let tokens =
    List.map
      (fun ((code_block : Code_block.t), loc) ->
        let pre_text =
          Document.Text
            (slice lines ~start:!cursor ~end_:code_block.location.start)
        in
        let block =
          match make_block ~loc code_block with
          | Ok block -> Document.Block block
          | Error (`Msg msg) ->
              failwith (Fmt.str "Error creating block: %s" msg)
        in
        let opening, closing = code_block_markup code_block in
        cursor := code_block.location.end_;
        [ pre_text ] @ opening @ [ block ] @ closing)
      code_blocks
    |> List.concat
  in
  let eof =
    {
      Odoc_parser.Loc.line = Array.length lines;
      column = String.length lines.(Array.length lines - 1);
    }
  in
  let eof_is_beyond_location (loc : Odoc_parser.Loc.point) =
    eof.line > loc.line || (eof.line = loc.line && eof.column > loc.column)
  in
  if eof_is_beyond_location !cursor then
    let remainder = slice lines ~start:!cursor ~end_:eof in
    if not (String.equal remainder "") then tokens @ [ Text remainder ]
    else tokens
  else tokens

let parse_mli file_contents =
  try Result.Ok (parse_mli file_contents)
  with exn -> Util.Result.errorf "%s" (Printexc.to_string exn)

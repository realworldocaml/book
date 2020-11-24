{

let unescape_word : string -> string = fun s ->
  (* The common case is that there are no escape sequences. *)
  match String.index s '\\' with
  | exception Not_found -> s
  | _ ->
    let buffer = Buffer.create (String.length s) in
    let rec scan_word index =
      if index >= String.length s then
        ()
      else
        let c = s.[index] in
        let c, increment =
          match c with
          | '\\' ->
            if index + 1 < String.length s then
              match s.[index + 1] with
              | '{' | '}' | '[' | ']' | '@' as c -> c, 2
              | _ -> c, 1
            else c, 1
          | _ -> c, 1
        in
        Buffer.add_char buffer c;
        scan_word (index + increment)
    in
    scan_word 0;
    Buffer.contents buffer



(* This is used for code and verbatim blocks. It can be done with a regular
   expression, but the regexp gets quite ugly, so a function is easier to
   understand. *)
let trim_leading_blank_lines : string -> string = fun s ->
  let rec scan_for_last_newline : int -> int -> int =
      fun index trim_until ->
    if index >= String.length s then
      String.length s
    else
      match s.[index] with
      | ' ' | '\t' | '\r' -> scan_for_last_newline (index + 1) trim_until
      | '\n' -> scan_for_last_newline (index + 1) (index + 1)
      | _ -> trim_until
  in
  let trim_until = scan_for_last_newline 0 0 in
  String.sub s trim_until (String.length s - trim_until)

let trim_trailing_blank_lines : string -> string = fun s ->
  let rec scan_for_last_newline : int -> int option -> int option =
      fun index trim_from ->
    if index < 0 then
      Some 0
    else
      match s.[index] with
      | ' ' | '\t' | '\r' -> scan_for_last_newline (index - 1) trim_from
      | '\n' -> scan_for_last_newline (index - 1) (Some index)
      | _ -> trim_from
  in
  let last = String.length s - 1 in
  match scan_for_last_newline last None with
  | None ->
    s
  | Some trim_from ->
    let trim_from =
      if trim_from > 0 && s.[trim_from - 1] = '\r' then
        trim_from - 1
      else
        trim_from
    in
    String.sub s 0 trim_from

let trim_leading_whitespace : first_line_offset:int -> string -> string =
 fun ~first_line_offset s ->
  let count_leading_whitespace line =
    let rec count_leading_whitespace' index =
      match line.[index] with
      | ' ' | '\t' -> count_leading_whitespace' (index + 1)
      | _ -> `Leading_whitespace index
      | exception Invalid_argument _ -> `Blank_line "\n"
    in
    count_leading_whitespace' 0
  in

  let lines = Astring.String.cuts ~sep:"\n" s in

  let least_amount_of_whitespace =
    List.fold_left (fun least_so_far line ->
      match (count_leading_whitespace line, least_so_far) with
      | (`Leading_whitespace n, None) -> Some n
      | (`Leading_whitespace n, Some least) when n < least -> Some n
      | _ -> least_so_far)
  in

  let first_line_max_drop, least_amount_of_whitespace =
    match lines with
    | [] -> 0, None
    | first_line :: tl ->
      begin match count_leading_whitespace first_line with
        | `Leading_whitespace n ->
          n, least_amount_of_whitespace (Some (first_line_offset + n)) tl
        | `Blank_line _ ->
          0, least_amount_of_whitespace None tl
      end
  in

  match least_amount_of_whitespace with
  | None ->
    s
  | Some least_amount_of_whitespace ->
    let drop n line =
      (* Since blank lines were ignored when calculating
         [least_amount_of_whitespace], their length might be less than the
         amount. *)
      if String.length line < n then line
      else String.sub line n (String.length line - n)
    in
    let lines =
      match lines with
      | [] -> []
      | first_line :: tl ->
        drop (min first_line_max_drop least_amount_of_whitespace) first_line
        :: List.map (drop least_amount_of_whitespace) tl
    in
    String.concat "\n" lines



module Location = Odoc_model.Location_
module Error = Odoc_model.Error



type input = {
  file : string;
  offset_to_location : int -> Location.point;
  warnings : Error.warning_accumulator;
  lexbuf : Lexing.lexbuf;
}

let with_location_adjustments
    k input ?start_offset ?adjust_start_by ?end_offset ?adjust_end_by value =

  let start =
    match start_offset with
    | None -> Lexing.lexeme_start input.lexbuf
    | Some s -> s
  in
  let start =
    match adjust_start_by with
    | None -> start
    | Some s -> start + String.length s
  in
  let end_ =
    match end_offset with
    | None -> Lexing.lexeme_end input.lexbuf
    | Some e -> e
  in
  let end_ =
    match adjust_end_by with
    | None -> end_
    | Some s -> end_ - String.length s
  in
  let location = {
    Odoc_model.Location_.file = input.file;
    start = input.offset_to_location start;
    end_ = input.offset_to_location end_;
  }
  in
  k input location value

let emit =
  with_location_adjustments (fun _ -> Location.at)

let warning =
  with_location_adjustments (fun input location error ->
    Error.warning input.warnings (error location))

let reference_token start target =
  match start with
  | "{!" -> `Simple_reference target
  | "{{!" -> `Begin_reference_with_replacement_text target
  | "{:" -> `Simple_link target
  | "{{:" -> `Begin_link_with_replacement_text target
  | _ -> assert false



let trim_leading_space_or_accept_whitespace input start_offset text =
  match text.[0] with
  | ' ' -> String.sub text 1 (String.length text - 1)
  | '\t' | '\r' | '\n' -> text
  | exception Invalid_argument _ -> ""
  | _ ->
    warning
      input
      ~start_offset
      ~end_offset:(start_offset + 2)
      Parse_error.no_leading_whitespace_in_verbatim;
    text

let trim_trailing_space_or_accept_whitespace text =
  match text.[String.length text - 1] with
  | ' ' -> String.sub text 0 (String.length text - 1)
  | '\t' | '\r' | '\n' -> text
  | _ -> text

let emit_verbatim input start_offset buffer =
  let t = Buffer.contents buffer in
  let t = trim_trailing_space_or_accept_whitespace t in
  let t = trim_leading_space_or_accept_whitespace input start_offset t in
  let t = trim_leading_blank_lines t in
  let t = trim_trailing_blank_lines t in
  emit input (`Verbatim t) ~start_offset

let emit_code_block input c =
  let c = trim_trailing_blank_lines c in
  let c =
    with_location_adjustments
      (fun _ location c ->
         let first_line_offset = location.start.column + 2 (* Length of {[ *) in
         trim_leading_whitespace ~first_line_offset c)
      input c
  in
  let c = trim_leading_blank_lines c in
  emit input (`Code_block c)



let heading_level input level =
  if String.length level >= 2 && level.[0] = '0' then begin
    warning
      input ~start_offset:1 (Parse_error.leading_zero_in_heading_level level)
  end;
  int_of_string level

}



let markup_char =
  ['{' '}' '[' ']' '@']
let space_char =
  [' ' '\t' '\n' '\r']
let bullet_char =
  ['-' '+']

let word_char =
  (_ # markup_char # space_char # bullet_char) | ('\\' markup_char)

let horizontal_space =
  [' ' '\t']
let newline =
  '\n' | "\r\n"

let reference_start =
  "{!" | "{{!" | "{:" | "{{:"

let code_block_text =
  ([^ ']'] | ']'+ [^ ']' '}'])* ']'*
let raw_markup =
  ([^ '%'] | '%'+ [^ '%' '}'])* '%'*
let raw_markup_target =
  ([^ ':' '%'] | '%'+ [^ ':' '%' '}'])* '%'*



rule token input = parse
  | horizontal_space* eof
    { emit input `End }

  | ((horizontal_space* newline as prefix)
    horizontal_space* ((newline horizontal_space*)+ as suffix) as ws)
    { emit input (`Blank_line ws) ~adjust_start_by:prefix ~adjust_end_by:suffix }

  | (horizontal_space* newline horizontal_space* as ws)
    { emit input (`Single_newline ws) }

  | (horizontal_space+ as ws)
    { emit input (`Space ws) }

  | (horizontal_space* (newline horizontal_space*)? as p) '}'
    { emit input `Right_brace ~adjust_start_by:p }

  | word_char (word_char | bullet_char | '@')*
  | bullet_char (word_char | bullet_char | '@')+ as w
    { emit input (`Word (unescape_word w)) }

  | '['
    { code_span
        (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }

  | '-'
    { emit input `Minus }

  | '+'
    { emit input `Plus }

  | "{b"
    { emit input (`Begin_style `Bold) }

  | "{i"
    { emit input (`Begin_style `Italic) }

  | "{e"
    { emit input (`Begin_style `Emphasis) }

  | "{^"
    { emit input (`Begin_style `Superscript) }

  | "{_"
    { emit input (`Begin_style `Subscript) }

  | "{!modules:" ([^ '}']* as modules) '}'
    { emit input (`Modules modules) }

  | (reference_start as start) ([^ '}']* as target) '}'
    { emit input (reference_token start target) }

  | "{[" (code_block_text as c) "]}"
    { emit_code_block input c }

  | "{v"
    { verbatim
        (Buffer.create 1024) None (Lexing.lexeme_start lexbuf) input lexbuf }

  | "{%" ((raw_markup_target as target) ':')? (raw_markup as s)
    ("%}" | eof as e)
    { let token = `Raw_markup (target, s) in
      if e <> "%}" then
        warning
          input
          ~start_offset:(Lexing.lexeme_end lexbuf)
          (Parse_error.not_allowed
            ~what:(Token.describe `End)
            ~in_what:(Token.describe token));
      emit input token }

  | "{ul"
    { emit input (`Begin_list `Unordered) }

  | "{ol"
    { emit input (`Begin_list `Ordered) }

  | "{li"
    { emit input (`Begin_list_item `Li) }

  | "{-"
    { emit input (`Begin_list_item `Dash) }

  | '{' (['0'-'9']+ as level) ':' (([^ '}'] # space_char)* as label)
    { emit
        input (`Begin_section_heading (heading_level input level, Some label)) }

  | '{' (['0'-'9']+ as level)
    { emit input (`Begin_section_heading (heading_level input level, None)) }

  | "@author" ((horizontal_space+ [^ '\r' '\n']*)? as author)
    { emit input (`Tag (`Author author)) }

  | "@deprecated"
    { emit input (`Tag `Deprecated) }

  | "@param" horizontal_space+ ((_ # space_char)+ as name)
    { emit input (`Tag (`Param name)) }

  | "@raise" horizontal_space+ ((_ # space_char)+ as name)
    { emit input (`Tag (`Raise name)) }

  | "@return"
    { emit input (`Tag `Return) }

  | "@see" horizontal_space* '<' ([^ '>']* as url) '>'
    { emit input (`Tag (`See (`Url, url))) }

  | "@see" horizontal_space* '\'' ([^ '\'']* as filename) '\''
    { emit input (`Tag (`See (`File, filename))) }

  | "@see" horizontal_space* '"' ([^ '"']* as name) '"'
    { emit input (`Tag (`See (`Document, name))) }

  | "@since" ((horizontal_space+ [^ '\r' '\n']*)? as version)
    { emit input (`Tag (`Since version)) }

  | "@before" horizontal_space+ ((_ # space_char)+ as version)
    { emit input (`Tag (`Before version)) }

  | "@version" ((horizontal_space+ [^ '\r' '\n']*)? as version)
    { emit input (`Tag (`Version version)) }

  | "@canonical" ((horizontal_space+ [^ '\r' '\n']*)? as identifier)
    { emit input (`Tag (`Canonical identifier)) }

  | "@inline"
    { emit input (`Tag `Inline) }

  | "@open"
    { emit input (`Tag `Open) }

  | "@closed"
    { emit input (`Tag `Closed) }



  | '{'
    { try bad_markup_recovery (Lexing.lexeme_start lexbuf) input lexbuf
      with Failure _ ->
        warning
          input
          (Parse_error.bad_markup
            "{" ~suggestion:"escape the brace with '\\{'.");
        emit input (`Word "{") }

  | ']'
    { warning input Parse_error.unpaired_right_bracket;
      emit input (`Word "]") }

  | "@param"
    { warning input Parse_error.truncated_param;
      emit input (`Tag (`Param "")) }

  | "@raise"
    { warning input Parse_error.truncated_raise;
      emit input (`Tag (`Raise "")) }

  | "@before"
    { warning input Parse_error.truncated_before;
      emit input (`Tag (`Before "")) }

  | "@see"
    { warning input Parse_error.truncated_see;
      emit input (`Word "@see") }

  | '@' ['a'-'z' 'A'-'Z']+ as tag
    { warning input (Parse_error.unknown_tag tag);
      emit input (`Word tag) }

  | '@'
    { warning input Parse_error.stray_at;
      emit input (`Word "@") }

  | '\r'
    { warning input Parse_error.stray_cr;
      token input lexbuf }

  | "{!modules:" ([^ '}']* as modules) eof
    { warning
        input
        ~start_offset:(Lexing.lexeme_end lexbuf)
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (`Modules "")));
      emit input (`Modules modules) }

  | (reference_start as start) ([^ '}']* as target) eof
    { warning
        input
        ~start_offset:(Lexing.lexeme_end lexbuf)
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (reference_token start "")));
      emit input (reference_token start target) }

  | "{[" (code_block_text as c) eof
    { warning
        input
        ~start_offset:(Lexing.lexeme_end lexbuf)
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (`Code_block "")));
      emit_code_block input c }



and code_span buffer nesting_level start_offset input = parse
  | ']'
    { if nesting_level = 0 then
        emit input (`Code_span (Buffer.contents buffer)) ~start_offset
      else begin
        Buffer.add_char buffer ']';
        code_span buffer (nesting_level - 1) start_offset input lexbuf
      end }

  | '['
    { Buffer.add_char buffer '[';
      code_span buffer (nesting_level + 1) start_offset input lexbuf }

  | '\\' ('[' | ']' as c)
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset input lexbuf }

  | newline newline
    { warning
        input
        (Parse_error.not_allowed
          ~what:(Token.describe (`Blank_line "\n\n"))
          ~in_what:(Token.describe (`Code_span "")));
      Buffer.add_char buffer '\n';
      code_span buffer nesting_level start_offset input lexbuf }

  | eof
    { warning
        input
        (Parse_error.not_allowed
          ~what:(Token.describe `End)
          ~in_what:(Token.describe (`Code_span "")));
      emit input (`Code_span (Buffer.contents buffer)) ~start_offset }

  | _ as c
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset input lexbuf }



and verbatim buffer last_false_terminator start_offset input = parse
  | (space_char as c) "v}"
    { Buffer.add_char buffer c;
      emit_verbatim input start_offset buffer }

  | "v}"
    { Buffer.add_string buffer "v}";
      verbatim
        buffer (Some (Lexing.lexeme_start lexbuf)) start_offset input lexbuf }

  | eof
    { begin match last_false_terminator with
      | None ->
        warning
          input
          (Parse_error.not_allowed
            ~what:(Token.describe `End)
            ~in_what:(Token.describe (`Verbatim "")))
      | Some location ->
        warning
          input
          ~start_offset:location
          ~end_offset:(location + 2)
          Parse_error.no_trailing_whitespace_in_verbatim
      end;
      emit_verbatim input start_offset buffer }

  | _ as c
    { Buffer.add_char buffer c;
      verbatim buffer last_false_terminator start_offset input lexbuf }



and bad_markup_recovery start_offset input = parse
  | [^ '}']+ as text '}' as rest
    { let suggestion =
        Printf.sprintf "did you mean '{!%s}' or '[%s]'?" text text in
      warning
        input
        ~start_offset
        (Parse_error.bad_markup ("{" ^ rest) ~suggestion);
      emit input (`Code_span text) ~start_offset}

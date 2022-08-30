module Ast = Ast
module Loc = Loc
module Warning = Warning

type t = {
  ast : Ast.t;
  warnings : Warning.t list;
  reversed_newlines : (int * int) list;
  original_pos : Lexing.position;
}

(* odoc uses an ocamllex lexer. The "engine" for such lexers is the standard
   [Lexing] module.

   As the [Lexing] module reads the input, it keeps track of only the byte
   offset into the input. It is normally the job of each particular lexer
   implementation to decide which character sequences count as newlines, and
   keep track of line/column locations. This is usually done by writing several
   extra regular expressions, and calling [Lexing.new_line] at the right time.

   Keeping track of newlines like this makes the odoc lexer somewhat too
   diffiult to read, however. To factor the aspect of keeping track of newlines
   fully out of the odoc lexer, instead of having it keep track of newlines as
   it's scanning the input, the input is pre-scanned before feeding it into the
   lexer. A table of all the newlines is assembled, and used to convert offsets
   into line/column pairs after the lexer emits tokens.

   [reversed_newlines ~input ~comment_location offset] returns a list of pairs
   of (line number * offset), allowing the easy conversion from the byte
   [offset], relative to the beginning of a comment, into a location, relative
   to the beginning of the file containing the comment. This can then be used
   to convert from byte offset to line number / column number - a Loc.point,
   and additionally for converting back from a Loc.point to a Lexing.position.
*)

let reversed_newlines : input:string -> (int * int) list =
 fun ~input ->
  let rec find_newlines line_number input_index newlines_accumulator =
    if input_index >= String.length input then newlines_accumulator
    else if
      (* This is good enough to detect CR-LF also. *)
      input.[input_index] = '\n'
    then
      find_newlines (line_number + 1) (input_index + 1)
        ((line_number + 1, input_index + 1) :: newlines_accumulator)
    else find_newlines line_number (input_index + 1) newlines_accumulator
  in
  find_newlines 1 0 [ (1, 0) ]

(* [offset_to_location] converts from an offset within the comment text, where
   [reversed_newlines] is the result of the above function and [comment_location]
   is the location of the comment within its file. The function is meant to be
   partially applied to its first two arguments, at which point it is passed to
   the lexer, so it can apply the table to its emitted tokens. *)

let offset_to_location :
    reversed_newlines:(int * int) list ->
    comment_location:Lexing.position ->
    int ->
    Loc.point =
 fun ~reversed_newlines ~comment_location byte_offset ->
  let rec scan_to_last_newline reversed_newlines_prefix =
    match reversed_newlines_prefix with
    | [] -> assert false
    | (line_in_comment, line_start_offset) :: prefix ->
        if line_start_offset > byte_offset then scan_to_last_newline prefix
        else
          let column_in_comment = byte_offset - line_start_offset in
          let line_in_file =
            line_in_comment + comment_location.Lexing.pos_lnum - 1
          in
          let column_in_file =
            if line_in_comment = 1 then
              column_in_comment + comment_location.Lexing.pos_cnum
              - comment_location.Lexing.pos_bol
            else column_in_comment
          in
          { Loc.line = line_in_file; column = column_in_file }
  in
  scan_to_last_newline reversed_newlines

(* Given a Loc.point and the result of [parse_comment], this function returns
   a valid Lexing.position *)
let position_of_point : t -> Loc.point -> Lexing.position =
 fun v point ->
  let { reversed_newlines; original_pos; _ } = v in
  let line_in_comment = point.Loc.line - original_pos.pos_lnum + 1 in
  let rec find_pos_bol reversed_newlines_prefix =
    match reversed_newlines_prefix with
    | [] -> assert false
    | [ _ ] -> original_pos.pos_bol
    | (line_number, line_start_offset) :: prefix ->
        if line_number > line_in_comment then find_pos_bol prefix
        else line_start_offset + original_pos.pos_cnum
  in
  let pos_bol = find_pos_bol reversed_newlines in
  let pos_lnum = point.Loc.line in
  let pos_cnum = point.column + pos_bol in
  let pos_fname = original_pos.pos_fname in
  { Lexing.pos_bol; pos_lnum; pos_cnum; pos_fname }

(* The main entry point for this module *)
let parse_comment ~location ~text =
  let warnings = ref [] in
  let reversed_newlines = reversed_newlines ~input:text in
  let token_stream =
    let lexbuf = Lexing.from_string text in
    let offset_to_location =
      offset_to_location ~reversed_newlines ~comment_location:location
    in
    let input : Lexer.input =
      { file = location.Lexing.pos_fname; offset_to_location; warnings; lexbuf }
    in
    Stream.from (fun _token_index -> Some (Lexer.token input lexbuf))
  in
  let ast, warnings = Syntax.parse warnings token_stream in
  { ast; warnings; reversed_newlines; original_pos = location }

(* Accessor functions, as [t] is opaque *)
let warnings t = t.warnings
let ast t = t.ast

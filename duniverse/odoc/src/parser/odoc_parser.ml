module Ast = Ast


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

   [offset_to_location ~input ~comment_location offset] converts the byte
   [offset], relative to the beginning of a comment, into a location, relative
   to the beginning of the file containing the comment. [input] is the comment
   text, and [comment_location] is the location of the comment within its file.
   The function is meant to be partially applied to its first two arguments, at
   which point it creates the table described above. The remaining function is
   then passed to the lexer, so it can apply the table to its emitted tokens. *)
let offset_to_location
    : input:string -> comment_location:Lexing.position ->
        (int -> Odoc_model.Location_.point) =
    fun ~input ~comment_location ->

  let rec find_newlines line_number input_index newlines_accumulator =
    if input_index >= String.length input then
      newlines_accumulator
    else
      (* This is good enough to detect CR-LF also. *)
      if input.[input_index] = '\n' then
        find_newlines
          (line_number + 1) (input_index + 1)
          ((line_number + 1, input_index + 1)::newlines_accumulator)
      else
        find_newlines line_number (input_index + 1) newlines_accumulator
  in

  let reversed_newlines : (int * int) list =
    find_newlines 1 0 [(1, 0)] in

  fun byte_offset ->
    let rec scan_to_last_newline reversed_newlines_prefix =
      match reversed_newlines_prefix with
      | [] ->
        assert false
      | (line_in_comment, line_start_offset)::prefix ->
        if line_start_offset > byte_offset then
          scan_to_last_newline prefix
        else
          let column_in_comment = byte_offset - line_start_offset in
          let line_in_file =
            line_in_comment + comment_location.Lexing.pos_lnum - 1 in
          let column_in_file =
            if line_in_comment = 1 then
              column_in_comment +
                comment_location.Lexing.pos_cnum -
                comment_location.Lexing.pos_bol
            else
              column_in_comment
          in
          {Odoc_model.Location_.line = line_in_file; column = column_in_file}
    in
    scan_to_last_newline reversed_newlines



let make_parser ~location ~text parse =
  Odoc_model.Error.accumulate_warnings begin fun warnings ->
    let token_stream =
      let lexbuf = Lexing.from_string text in
      let offset_to_location =
        offset_to_location ~input:text ~comment_location:location in
      let input : Lexer.input =
        {
          file = location.Lexing.pos_fname;
          offset_to_location;
          warnings;
          lexbuf;
        }
      in
      Stream.from (fun _token_index -> Some (Lexer.token input lexbuf))
    in
    parse warnings token_stream
  end


let parse_comment_raw ~location ~text =
  make_parser ~location ~text Syntax.parse


let parse_comment ~sections_allowed ~containing_definition ~location ~text =
  make_parser ~location ~text (fun warnings token_stream ->
      Syntax.parse warnings token_stream
      |> Semantics.ast_to_comment
         warnings ~sections_allowed ~parent_of_sections:containing_definition
    )

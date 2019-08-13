(** Text wrapping and filling for OCaml. *)

type t = {
  width : int;
  initial_indent : string;
  subsequent_indent : string;
  expand_tabs : bool;
  replace_whitespace : bool;
  fix_sentence_endings : bool;
  break_long_words : bool;
  break_on_hyphens : bool;
  drop_whitespace : bool
}

(* A simple regex that just split on recognized spaces. E. g.
       "Hello there -- you goof-ball, use the -b option!"
   splits into
       Hello/ /there/ /--/ /you/ /goof-ball,/ /use/ /the/ /-b/ /option!/ *)
let wordsep_simple_re = Str.regexp "[\r\n\t ]+"

and whitespace_re = Str.regexp "\t\n\x0b\x0c\r"

and sentence_end_re = Str.regexp "[a-z][.!?][\"\']?$"

(** Munge whitespace in text: expand tabs and convert all other
    whitespace characters to spaces.  Eg. " foo\tbar\n\nbaz"
    becomes " foo    bar  baz". *)
let munge_whitespace { expand_tabs; replace_whitespace; _ } text =
  let text =
    if expand_tabs then
      Str.global_replace (Str.regexp "\t") (String.make 8 ' ') text
    else
      text
  in

  if replace_whitespace then
    Str.global_replace whitespace_re " " text
  else
    text

(** Split the text to wrap into indivisible chunks. Currently
    the chunks are the same as words.

    TODO(superbobry): add support for [break_on_hyphens], see
    http://hg.python.org/cpython/file/ca2a35140e6a/Lib/textwrap.py#l75 *)
and split _w s =
  let res = Str.full_split wordsep_simple_re s in
  List.map (function
    | Str.Delim _ -> " "
    | Str.Text t  -> t
  ) res

(** Correct sentence endings buried in [chunks].  Eg. when the
    original text contains "... foo.\nBar ...", munge_whitespace()
    and split() will convert that to [..., "foo.", " ", "Bar", ...]
    which has one too few spaces; this method simply changes the one
    space to two. *)
and fix_sentence_endings _w chunks =
  let chunks = Array.of_list chunks in
  let patsearch s =
    try
      Str.search_forward sentence_end_re s 0 >= 0
    with Not_found ->
      false
  in

  let rec inner i chunks =
    if i >= Array.length chunks - 1 then
      chunks
    else
      if chunks.(i + 1) = " " && patsearch chunks.(i) then
        (chunks.(i + 1) <- "  "; inner (i + 2) chunks)
      else
        inner (i + 1) chunks
  in

  Array.to_list (inner 0 chunks)

(** Handle a chunk of text (most likely a word, not whitespace) that
    is too long to fit in any line. *)
and handle_long_word w chunks cur_line cur_len width =
  let space_left = if width < 1 then 1 else width - cur_len in
  match chunks with
    | (chunk :: chunks) when w.break_long_words ->
      (String.sub chunk 0 space_left :: cur_line,
       String.sub chunk space_left (String.length chunk - space_left) :: chunks)
    | (chunk :: chunks) when cur_line = [] ->
      (chunk :: cur_line, chunks)
    | chunks ->
      (cur_line, chunks)

(** Wrap a sequence of text chunks and return a list of lines of
    length [w.width] or less.  (If [break_long_words] is false,
    some lines may be longer than this.)  Chunks correspond
    to words and the whitespace between them: each chunk is
    indivisible (modulo [break_long_words]), but a line break can
    come between any two chunks.  Chunks should not have internal
    whitespace; ie. a chunk is either all whitespace or a "word".
    Whitespace chunks will be removed from the beginning and end of
    lines, but apart from that whitespace is preserved. *)
let wrap_chunks w =
  let is_whitespace = function
    | "" -> true
    | s  -> Str.string_match wordsep_simple_re s 0
  in

  let pre_drop_whitespace lines = function
    | (chunk :: chunks) when
        w.drop_whitespace && lines <> [] && is_whitespace chunk ->
      chunks
    | chunks -> chunks
  in

  let post_drop_whitespace = function
    | (chunk :: line) when
        w.drop_whitespace && is_whitespace chunk -> line
    | line -> line
  in

  let rec current line len width = function
    | chunk :: chunks when String.length chunk + len <= width ->
      current (chunk :: line) (len + String.length chunk) width chunks
    | chunks ->
      (line, len, chunks)
  in

  let rec inner lines = function
    | []     -> List.rev lines
    | chunks ->
      let indent = match lines with
        | [] -> w.initial_indent
        | _  -> w.subsequent_indent
      in

      (* Maximum width for the line, being built. *)
      let width = w.width - String.length indent in

      (* First chunk on line is whitespace -- drop it, unless this
         is the very beginning of the text. *)
      let chunks = pre_drop_whitespace lines chunks in

      (* Fill current line with chunks to fit maximum width. *)
      let (cur_line, cur_len, chunks) = current [] 0 width chunks in

      (* The current line is full, and the next chunk is too big to
         fit on *any* line (not just this one). *)
      let (cur_line, chunks) =
        if chunks <> [] && String.length (List.hd chunks) > width then
          handle_long_word w chunks cur_line cur_len width
        else
          (cur_line, chunks)
      in

      (* If the last chunk on this line is all whitespace -- drop it! *)
      match post_drop_whitespace cur_line with
        | [] -> inner lines chunks
        | cur_line ->
          inner
            ((indent ^ String.concat "" (List.rev cur_line)) :: lines)
            chunks
  in inner []


(* +------------+
   | Public API |
   +------------+ *)

let make
    ?(initial_indent="")
    ?(subsequent_indent="")
    ?(expand_tabs=true)
    ?(replace_whitespace=true)
    ?(fix_sentence_endings=false)
    ?(break_long_words=true)
    ?(break_on_hyphens=true)
    ?(drop_whitespace=true) width =
  if width <= 0 then
    raise (Invalid_argument "width <= 0");

  { width; initial_indent; subsequent_indent; expand_tabs;
    replace_whitespace; fix_sentence_endings;
    break_long_words; break_on_hyphens; drop_whitespace
  }

let wrap w text =
  let chunks = split w (munge_whitespace w text) in
  let chunks =
    if w.fix_sentence_endings then
      fix_sentence_endings w chunks
    else
      chunks
  in wrap_chunks w chunks

let fill w text =
  String.concat "\n" (wrap w text)

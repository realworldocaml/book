(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* This lexer is used to cut an input into segments, delimited by a blank
   line. (More precisely, by a run of at least one blank line and zero or more
   comment lines.) It produces a list of segments, where each segment is
   represented as a pair of positions. It is stand-alone and cannot fail. *)

(* The whitespace in between two segments can contain comments, and the user
   may wish to preserve them. For this reason, we view a run of whitespace as
   a segment, too, and we accompany each segment with a tag which is either
   [Segment] or [Whitespace]. The two kinds of segments must alternate in the
   list that we produce. *)

{

  type tag =
    | Segment
    | Whitespace

  open Lexing

}

let newline    = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ';' ]

let comment    = '#' [^'\010''\013']* newline

(* In the idle state, we skip whitespace, newlines and comments
   (while updating the liner counter). If we reach the end of file,
   we return the list of all segments found so far. If we reach a
   non-blank non-comment character, we record its position and
   switch to the busy state. *)

rule idle opening segments = parse
| whitespace
    { idle opening segments lexbuf }
| newline
    { new_line lexbuf; idle opening segments lexbuf }
| comment
    { new_line lexbuf; idle opening segments lexbuf }
| eof
    { let closing = lexbuf.lex_start_p in
      let segment = Whitespace, opening, closing in
      let segments = segment :: segments in
      List.rev segments }
| _
    { let closing = lexbuf.lex_start_p in
      let segment = Whitespace, opening, closing in
      let segments = segment :: segments in
      let opening = closing in
      busy segments opening false lexbuf }

(* In the busy state, we skip everything, maintaining one bit
   [just_saw_a_newline], until [just_saw_a_newline] is true
   and we find a second newline. This marks the end of a
   segment, and we revert back to the idle state. If we
   reach the end of file, we consider that this is also
   the end of a segment. *)

and busy segments opening just_saw_a_newline = parse
| whitespace
    { busy segments opening just_saw_a_newline lexbuf }
| newline
    { new_line lexbuf;
      (* The newline that we just saw is already included in the segment.
         This one is not included. *)
      let closing = lexbuf.lex_start_p in
      if just_saw_a_newline then
        let segment = Segment, opening, closing in
        let segments = segment :: segments in
        let opening = closing in
        idle opening segments lexbuf
      else
        busy segments opening true lexbuf }
| eof
    { let closing = lexbuf.lex_start_p in
      let segment = Segment, opening, closing in
      let segments = segment :: segments in
      List.rev segments }
| _
    { busy segments opening false lexbuf }

{

  (* This wrapper function reads a file, cuts it into segments, and
     creates a fresh lexbuf for each segment, taking care to adjust
     its start position. *)

  let segment filename : (tag * string * lexbuf) list =
    let content = IO.read_whole_file filename in
    let lexbuf = from_string content in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let segments : (tag * position * position) list =
      idle lexbuf.lex_curr_p [] lexbuf
    in
    List.map (fun (tag, startp, endp) ->
      let start = startp.pos_cnum in
      let length = endp.pos_cnum - start in
      let content = String.sub content start length in
      let lexbuf = from_string content in
      lexbuf.lex_start_p <- startp;
      lexbuf.lex_curr_p <- startp;
      lexbuf.lex_abs_pos <- startp.pos_cnum;
        (* That was tricky to find out. See [Lexing.engine]. [pos_cnum] is
           updated based on [buf.lex_abs_pos + buf.lex_curr_pos]. *)
      tag, content, lexbuf
    ) segments

}


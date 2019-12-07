(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



{

type error =
  | Unexpected_end_of_file

let string_of_error = function
  | Unexpected_end_of_file -> "unexpected end of file"

exception Exception of int * error

let () =
  Printexc.register_printer
    (function
      | Exception (l, m) ->
          let msg = Printf.sprintf "lexing error at line %d: %s" l (string_of_error m) in
          Some msg
      | _ -> None)

let fail lexbuf error =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  raise (Exception (pos.pos_lnum, error))

let incr_line lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_lnum = succ pos.pos_lnum;
                         pos_bol = pos.pos_cnum }

let get_line lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  pos.pos_lnum

let report_unmatched () =
  prerr_endline "unmatched '(*BISECT-IGNORE-END*)' comment"

let report_unmatched_pair () =
  prerr_endline "unmatched '(*BISECT-IGNORE-BEGIN*)' and '(*BISECT-IGNORE-END*)' comments"

(* From the OCaml source code *)
let update_loc lexbuf opt_file line =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match opt_file with
                 | None -> pos.Lexing.pos_fname
                 | Some f -> f
  in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_fname = new_file;
    Lexing.pos_lnum = line;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }

let name_to_new_state ignored marked stack (old_filename,acc as files) = function
  | Some new_filename when new_filename <> old_filename ->
      if not (Stack.is_empty stack) then report_unmatched_pair();
      let nacc = (old_filename, (ignored, marked))::acc in
      ([],[],Stack.create (),(new_filename, nacc))
  | _ -> (ignored, marked, stack, files)
}

let eol = ('\010' | '\013' |"\013\010" | "\010\013")

rule normal ignored marked stack files = parse
| "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
    ('\"' ([^ '\010' '\013' '\"']* as name) '\"')?
    [^ '\010' '\013']* '\010'
    { update_loc lexbuf name (int_of_string num);
      let (i,m,s,f) = name_to_new_state ignored marked stack files name in
      normal i m s f lexbuf
    }

| "'\"'"                    { normal ignored marked stack files lexbuf }
| "'\\\"'"                  { normal ignored marked stack files lexbuf }
| "\""                      { string 0 ignored marked stack files lexbuf }
| "(*BISECT-IGNORE-BEGIN*)" { let line = get_line lexbuf in
                              Stack.push line stack;
                              normal ignored marked stack files lexbuf }
| "(*BISECT-IGNORE-END*)"   { let ignored =
                                try
                                  let bib = Stack.pop stack in
                                  (bib, get_line lexbuf) :: ignored
                                with Stack.Empty ->
                                  report_unmatched ();
                                  ignored in
                              normal ignored marked stack files lexbuf }
| "(*BISECT-IGNORE*)"       { let line = get_line lexbuf in
                              normal ((line, line) :: ignored) marked stack files lexbuf }
| "(*BISECT-VISIT*)"        { normal ignored (get_line lexbuf :: marked) stack files lexbuf }
| "(*BISECT-MARK*)"         { normal ignored (get_line lexbuf :: marked) stack files lexbuf }
| "(*"                      { comment 1 ignored marked stack files lexbuf }
| eol                       { incr_line lexbuf;
                              normal ignored marked stack files lexbuf }
| eof                       { if not (Stack.is_empty stack) then report_unmatched_pair ();
                              let (filename,acc) = files in
                              (filename, (ignored, marked))::acc }
| _                         { normal ignored marked stack files lexbuf }

and string n ignored marked stack files = parse
| "\\\\"                    { string n ignored marked stack files lexbuf }
| "\\\""                    { string n ignored marked stack files lexbuf }
| "\""                      { if n = 0 then
                                normal ignored marked stack files lexbuf
                              else
                                comment n ignored marked stack files lexbuf }
| eol                       { incr_line lexbuf;
                              string n ignored marked stack files lexbuf }
| eof                       { fail lexbuf Unexpected_end_of_file }
| _                         { string n ignored marked stack files lexbuf }

and comment n ignored marked stack files = parse
| "(*"                      { comment (succ n) ignored marked stack files lexbuf }
| "*)"                      { if n = 1 then
                                normal ignored marked stack files lexbuf
                              else
                                comment (pred n) ignored marked stack files lexbuf }
| "'\"'"                    { comment n ignored marked stack files lexbuf }
| "'\\\"'"                  { comment n ignored marked stack files lexbuf }
| "\""                      { string n ignored marked stack files lexbuf }
| eol                       { incr_line lexbuf; comment n ignored marked stack files lexbuf }
| eof                       { fail lexbuf Unexpected_end_of_file }
| _                         { comment n ignored marked stack files lexbuf }

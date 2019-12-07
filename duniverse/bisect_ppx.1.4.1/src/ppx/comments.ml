(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



let no_comment_parsing = ref false

type t = {
    mutable ignored_intervals : (int * int) list;
    mutable marked_lines : int list;
  }

let no_comments = {
  ignored_intervals = [];
  marked_lines = [];
}

let comments_cache : (string, t) Hashtbl.t = Hashtbl.create 17

let get filename =
  if !no_comment_parsing then
    no_comments
  else
  try
    Hashtbl.find comments_cache filename
  with Not_found ->
    (* We many have multiple 'filenames' inside of a file
       because of the line directive. *)
    let chan = open_in filename in
    try
      let lexbuf = Lexing.from_channel chan in
      let stack = Stack.create () in
      let lst = Comments_lexer.normal [] [] stack (filename,[]) lexbuf in
      let as_comments =
        List.map (fun (filename, (ignored_intervals,marked_lines)) ->
          let comments = { ignored_intervals ; marked_lines } in
          Hashtbl.add comments_cache filename comments;
          (filename, comments)) lst
      in
      close_in_noerr chan;
      List.assoc filename as_comments
    with e ->
      close_in_noerr chan;
      raise e

let line_is_ignored line_number comments_info =
  List.exists
    (fun (low, high) -> line_number >= low && line_number <= high)
    comments_info.ignored_intervals
  || List.mem line_number comments_info.marked_lines

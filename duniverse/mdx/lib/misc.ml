(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Astring

let hpad_of_lines = function
  | []   -> 0
  | h::_ ->
    let i = ref 0 in
    while (!i < String.length h && h.[!i] = ' ') do incr i; done;
    !i

let pp_pad ppf = function
  | 0 -> ()
  | i -> Fmt.string ppf (String.v ~len:i (fun _ -> ' '))

let pp_lines pp = Fmt.(list ~sep:(unit "\n") pp)
let dump_string ppf s = Fmt.pf ppf "%S" s

let read_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let file_contents = really_input_string ic len in
  close_in ic;
  file_contents

let init file =
  let file_contents = read_file file in
  let lexbuf = Lexing.from_string file_contents in
  lexbuf.lex_curr_p <-
    { pos_fname = file
    ; pos_cnum  = 0
    ; pos_lnum  = 1
    ; pos_bol   = 0
    };
  file_contents, lexbuf

let run_expect_test ~force_output file ~f =
  let file_contents, lexbuf = init file in
  let expected = f file_contents lexbuf in
  let corrected_file = file ^ ".corrected" in
  if force_output || file_contents <> expected then begin
    let oc = open_out_bin corrected_file in
    output_string oc expected;
    close_out oc;
  end else begin
    if Sys.file_exists corrected_file then Sys.remove corrected_file
  end

let pp_position ppf lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  Fmt.pf ppf
    "File \"%s\", line %d, character %d"
    p.Lexing.pos_fname p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol)

(* TODO: better error reporting *)
let err lexbuf fmt =
  Fmt.kstrf (fun str ->
      Fmt.failwith "%a: %s" pp_position lexbuf str
    ) fmt

let parse_version v =
  let to_string = String.Sub.to_string in
  let to_int x = try Some (int_of_string x) with _ -> None in
  let is_dot c = c = '.' in
  match String.find is_dot v with
  | None -> to_int v, None, None
  | Some i_dot_1 ->
     let major = String.sub ~stop:i_dot_1 v |> to_string |> to_int in
     let remain = String.sub ~start:(i_dot_1+1) v |> to_string in
     match String.find is_dot remain with
     | None -> major, to_int remain, None
     | Some i_dot_2 ->
        let minor = String.sub ~stop:i_dot_2 remain |> to_string |> to_int in
        let remain = String.sub ~start:(i_dot_2+1) remain |> to_string in
        major, minor, to_int remain

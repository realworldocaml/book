open! Core
open! Import

module Trailing_newline = struct
  type t =
    [ `Missing_trailing_newline
    | `With_trailing_newline
    ]
  [@@deriving sexp_of]
end

let lines_of_contents contents =
  let lines = Array.of_list (String.split_lines contents) in
  let has_trailing_newline =
    let length = String.length contents in
    if length = 0 || Char.equal contents.[length - 1] '\n'
    then `With_trailing_newline
    else `Missing_trailing_newline
  in
  lines, has_trailing_newline
;;

let warn_if_no_trailing_newline
      ~warn_if_no_trailing_newline_in_both
      ~warn
      ~prev:(prev_file_newline, prev_file)
      ~next:(next_file_newline, next_file)
  =
  match prev_file_newline, next_file_newline with
  | `With_trailing_newline, `With_trailing_newline -> ()
  | `With_trailing_newline, `Missing_trailing_newline -> warn next_file
  | `Missing_trailing_newline, `With_trailing_newline -> warn prev_file
  | `Missing_trailing_newline, `Missing_trailing_newline ->
    if warn_if_no_trailing_newline_in_both
    then (
      warn prev_file;
      warn next_file)
;;

let binary_different_message
      ~(config : Configuration.t)
      ~prev_file
      ~prev_is_binary
      ~next_file
      ~next_is_binary
  =
  match config.location_style with
  | Diff | None | Separator ->
    sprintf
      !"Files %{File_name#hum}%s and %{File_name#hum}%s differ"
      prev_file
      (if prev_is_binary then " (binary)" else "")
      next_file
      (if next_is_binary then " (binary)" else "")
  | Omake ->
    sprintf
      !"%s\n  File \"%{File_name#hum}\"\n  binary files differ\n"
      (Format.Location_style.omake_style_error_message_start
         ~file:(File_name.display_name prev_file)
         ~line:1)
      next_file
;;

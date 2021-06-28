open! Core

let looks_like_python_filename = String.is_suffix ~suffix:".py"

let looks_like_python_first_line first_line =
  String.is_prefix first_line ~prefix:"#!"
  && String.is_substring first_line ~substring:"python"
;;

let looks_like_python ~file:filename ~lines ~first_line_of_lines =
  looks_like_python_filename filename
  || Option.exists (first_line_of_lines lines) ~f:looks_like_python_first_line
;;

let for_diff_internal ~file1 ~file2 ~lines1 ~lines2 ~first_line_of_lines =
  looks_like_python ~file:file1 ~lines:lines1 ~first_line_of_lines
  || looks_like_python ~file:file2 ~lines:lines2 ~first_line_of_lines
;;

let for_diff =
  for_diff_internal ~first_line_of_lines:(fun lines ->
    match String.lsplit2 lines ~on:'\n' with
    | Some (first_line, _) -> Some first_line
    | None -> Some lines)
;;

let for_diff_array =
  for_diff_internal ~first_line_of_lines:(fun lines ->
    if Array.is_empty lines then None else Some lines.(0))
;;

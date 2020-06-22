open! Core

(** Heuristics for whitespace-sensitive inputs (e.g., if it looks like Python). *)


val for_diff : file1:string -> file2:string -> lines1:string -> lines2:string -> bool

val for_diff_array
  :  file1:string
  -> file2:string
  -> lines1:string array
  -> lines2:string array
  -> bool

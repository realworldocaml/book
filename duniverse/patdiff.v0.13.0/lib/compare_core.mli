open! Core

val diff_files
  :  Configuration.t
  -> prev_file:string
  -> next_file:string
  -> [ `Different | `Same ]

val diff_dirs
  :  Configuration.t
  -> prev_dir:string
  -> next_dir:string
  -> file_filter:(string * Unix.stats -> bool) option
  -> [ `Different | `Same ]

(* diff strings and output to strings, supposed to be used by ocaml code *)

val diff_strings
  :  ?print_global_header:bool
  -> Configuration.t
  -> prev:Patdiff_core.diff_input
  -> next:Patdiff_core.diff_input
  -> [ `Different of string | `Same ]

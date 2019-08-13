(** Diff two files. Use [diff_command] to specify what command to use. If not specified
    [patdiff] is used, with a fallback to [diff -u] if [patdiff] produces no
    differences. *)
val print
  :  ?diff_command:string
  -> ?use_color:bool (** default: false *)
  -> file1:string
  -> file2:string
  -> unit
  -> unit

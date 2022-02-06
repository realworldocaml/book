(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** Functions used in several places in the reporter. *)



(** {1 Logging} *)

val verbose : bool ref
(** Whether {!Util.info} causes any output to be displayed. Set by
    [--verbose]. *)

val info : ('a, unit, string, unit) format4 -> 'a
(** Writes a message to STDERR if {!Util.verbose} is set. The message is
    prefixed with ["Info: "]. *)

val fatal : ('a, unit, string, 'b) format4 -> 'a
(** Writes a message to STDERR and exits the process with [1]. The message is
    prefixed with ["Error: "].

    [bisect-ppx-report] favors this kind of error handling because it is local
    and composable. All values needed to generate the error message are
    available in local scope, and the "handler" is near the code that causes the
    error. On the other hand, [bisect-ppx-report] is a simple enough program
    that it does not need to do any fancy error handling (the operating system
    will automatically close any open files when the process exits). So there is
    no need for a stack of exception handlers to respond to errors. *)



(** {1 General} *)

val split : ('a -> bool) -> 'a list -> ('a list * 'a list)
(** [split f list] splits [list] into a prefix and suffix. The suffix begins
    with the first element of [list] for which [f] evaluated to [false]. *)



(** {1 File system} *)

val mkdirs : string -> unit
(** Creates the given directory, and any necessary parent directories. Failure
    to create directory is considered fatal, and the function terminates the
    reporter process. *)

val find_dune_workspace_root : unit -> string option
(** Returns the directory containing the outermost [dune-workspace] file,
    relative to the current directory. *)

val find_source_file :
  source_roots:string list -> ignore_missing_files:bool -> filename:string ->
    string option
(** Attempts to find the given file relative to each of the given potential
    source roots. If the file cannot be found, either evaluates to [None] if
    [~ignore_missing_files:true], or terminates the process if
    [~ignore_missing_files:false]. *)



(** {1 Coverage statistics} *)

val line_counts :
  filename:string -> points:int array -> counts:int array -> int option list
(** Computes the visited lines for [~filename]. For each line, returns either:

    - [None], if there is no point on the line.
    - [Some count], where [count] is the number of visits to the least-visited
      point on the line. The count may be zero.

    This function is "lossy," as OCaml code often has multiple points on one
    line. However, this is a necessary conversion for line-based coverage report
    formats, such as Coveralls and Cobertura.

    This function reads the file with [~filename]. In case of an error, it
    terminates the process. The file's existence should already have been
    checked by {!Util.find_source_file}. An error in [line_counts] therefore
    suggests a permissions problem, a race condition with another process, or
    another abnormal situation. *)

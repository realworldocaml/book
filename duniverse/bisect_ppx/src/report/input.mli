(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



val load_coverage :
  coverage_files:string list ->
  coverage_paths:string list ->
  expect:string list ->
  do_not_expect:string list ->
    Bisect_common.coverage
(** Loads the given [~coverage_files], and any [.coverage] files found under the
    given [~coverage_paths]. Returns the per-source coverage data, accumulated
    across all the [.coverage] files.

    [~expect] is a list of expected source files and/or source directories that
    should appear in the returned coverage data. [~do_not_expect] subtracts some
    files and directories from [~expect].

    Any I/O errors that occur during this function are considered fatal, as the
    [.coverage] files have already been found. Failure to open such a file is
    probably a permissions error, a race condition with another process, or
    another serious and unusual condition. A missing source file, relative to
    the set given by [~expect], is also a fatal error, because the user has
    explicitly stated that the source file should be represented in the
    report. *)

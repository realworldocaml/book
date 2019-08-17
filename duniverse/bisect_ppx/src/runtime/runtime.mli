(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



(** This module provides runtime support for Bisect. Instrumented programs
    should hence be linked with this module.

    Each instrumented file creates an array of counters, one for each point in
    that file. It then registers the array with this runtime module. Upon
    program exit (using [at_exit]), this module dumps the accumulated counts
    from all the arrays into an output file.

    The default base name for the output file is [bisect] in the current
    directory, but another base name can be specified using the [BISECT_FILE]
    environment variable. The actual file name is the first non-existing
    [<base><n>.out] file where [base] is the base name and [n] a natural
    number value padded with zeroes to 4 digits (i.e. "0001", "0002", and
    so on).

    Another environment variable can be used to customize the behaviour of
    Bisect: [BISECT_SILENT]. If this variable is set to [YES] or [ON]
    (ignoring case), then Bisect will not output any message. Otherwise, Bisect
    will output a message in two situations:
    - when the output file cannot be created at program termination;
    - when the data cannot be written at program termination.
    If [BISECT_SILENT] is set to [ERR] (ignoring case), these error messages are
    routed to [stderr], otherwise [BISECT_SILENT] is used to determine a
    filename for this output. The default value is [bisect.log].

    Because instrumented modules refer to [Bisect], one is advised to link
    this module as one of the first ones of the program.

    Since the counts output file and log file are, by default, relative to the
    current working directory, an instrumented process should be careful about
    changing its working directory, or else [BISECT_FILE] and [BISECT_SILENT]
    should be specified with absolute paths. *)


val register_file :
  string -> point_count:int -> point_definitions:string ->
    [`Staged of (int -> unit)]
(** [register_file file ~point_count ~point_definitions] indicates that the file
    [file] is part of the application that has been instrumented.
    [point_definitions] is a serialized [Common.point_definition list] giving
    the locations of all points in the file. The returned callback is used to
    increment visitation counts. *)

val dump_counters_exn : out_channel -> unit
(** [dump_counters_exn channel] dumps the runtime coverage counters
    to the specified [channel].

    An exception is raised if writing is not successful *)

val reset_counters : unit -> unit
(** [reset_counters ()] will reset the runtime coverage counters. *)

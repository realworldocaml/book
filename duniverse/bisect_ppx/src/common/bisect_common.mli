(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** This module provides type definitions and functions that are shared between
    more than one part of Bisect, namely:

    - the instrumenter (PPX)
    - the native runtime ([Bisect.Runtime] in [src/runtime/native/])
    - the ReScript runtime ([Bisect.Runtime] in [src/runtime/js/])
    - the reporter ([bisect-ppx-report]) *)



(** {1 Types}

    Types representing accumulated visit counts. This data is written to
    [bisect*.coverage] files when an instrumented binary exits.

    The two types in this section, {!instrumented_file} and {!coverage}, and the
    value {!coverage_file_identifier}, are the only items shared with the
    reporter. Otherwise, the reporter is self-contained. *)

type instrumented_file = {
  filename : string; (** Source file name. *)
  points : int array; (** Byte offsets of the points placed in the file. *)
  counts : int array; (** Visitation counts, one for each point. *)
}
(** Data gathered for a single source file. *)

type coverage = (string, instrumented_file) Hashtbl.t
(** A binary instrumented with Bisect, when run, produces coverage statistics
    for each of its source files. The runtime and reporter both index the
    statistics by source file name. *)

val coverage_file_identifier : string
(** A string written at the beginning of each [bisect*.coverage] files. Provides
    a sanity check for the reporter that it is reading a [bisect*.coverage]
    file, and the file format version. *)



(** {1 Initialization} *)

val register_file :
  filename:string -> points:int array -> [`Visit of (int -> unit)]
(** Each source file is instrumented to call {!Bisect.Runtime.register_file} at
    run time, during program initialization. {!Bisect.Runtime.register_file}
    eventually forwards to this function, {!Biesct_common.register_file}. This
    function allocates the visit count array, with one array cell for each
    point, and registers the array for later writing to [bisect*.coverage]
    files.

    - [~filename] is the name of the source file.
    - [~points] is the list of byte offsets of the instrumentation points placed
      in the source file.

    The return value is the function that is called by each instrumentation
    point to increment its own visit count. The instrumentation point passes its
    own index to the function. *)



(** {1 [.coverage] output} *)

val write_runtime_coverage : coverage -> out_channel -> unit
(** Writes the [coverage] to the given output channel. *)

val write_runtime_data : out_channel -> unit
(** Writes the current accumulated coverage data (of type {!coverage}) to the
    given output channel. *)

val runtime_data_to_string : unit -> string option
(** Same as {!write_runtime_data}, but writes the output to a string instead.

    [None] is returned if there are no source files registered. This can occur
    when the runtime gets linked into a binary, but no files had been
    instrumented, because instrumentation was turned off. This combination
    normally shouldn't happen, but it can occur depending on the quality of the
    integration between the build system and Bisect. *)

val reset_counters : unit -> unit
(** Clears accumulated visit counts. All array cells are set to zero. *)

val random_filename : prefix:string -> string
(** Returns a random filename with the given prefix. *)

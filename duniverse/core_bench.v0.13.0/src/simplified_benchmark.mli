(** A module internal to [Core_Bench].

    Converts an analysis result to a simplified json representation *)

open! Core

module Field_type : sig

  type t =
    | Time_per_run
    | Minor_words_per_run
    | Major_words_per_run
    | Promoted_words_per_run

  include Equal.S      with type t := t
  include Stringable.S with type t := t

  val of_short_string : string -> t

  val to_short_string : t -> string

  (* converts [t] to a verbose name e.g. "Time (ns) Per Run" *)
  val to_label_string : t -> string

  val all : t list

end

module Result : sig
  type t =
    { (* [benchmark_name] is just the user defined name when defining an inline test
       e.g. "addition test" *)
      benchmark_name                  : string
    (* [benchmark_name_with_index] is the user defined name along with the index number
       for indexed tests e.g. "addition test:1000" *)
    ; benchmark_name_with_index       : string
    (* [full_benchmark_name] includes file, module name, given name, and index
       concatenated e.g. "[test.ml] addition test:1000" *)
    ; full_benchmark_name             : string
    (* [dup_id] is a unique id for benchmarks that have the exact same name *)
    ; dup_id                          : int option
    ; file_name                       : string
    ; module_name                     : string
    ; library_name                    : string
    (* version contains the full string received from Version_util.version
        *)
    ; version                         : string
    (* hg_revision contains only the action revision id used by hg
       e.g. "1e88f63603b3" *)
    ; hg_revision                     : string option
    ; hg_active_bookmark              : string option
    ; x_library_inlining              : bool
    ; ocaml_version                   : string
    (* machine_where_benchmark_was_run stores the name of the performance machine used
       for the benchmarks  *)
    ; machine_where_benchmark_was_run : string
    (* epoch_time_of_run is the epoch time of when exactly the benchmarks were run,
       in nanoseconds *)
    ; epoch_time_of_run               : Int63.t
    (* time_of_hg_revision is the time at which the hg revision was created *)
    ; time_of_hg_revision             : string option
    (* Various stats computed by bench. *)
    ; time_r_square                   : float
    ; time_per_run_nanos              : float
    ; ci95_upper_bound                : float
    ; ci95_lower_bound                : float
    ; minor_words_per_run             : float
    ; major_words_per_run             : float
    ; promoted_words_per_run          : float
    }
    [@@deriving typerep, sexp]
end

module Results : sig
  type t = Result.t list [@@deriving typerep, sexp]
end

val to_sexp : ?libname:string -> Analysis_result.t list -> Sexp.t

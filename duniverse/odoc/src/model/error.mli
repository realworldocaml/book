type t

val make :
  ?suggestion:string -> ('a, unit, string, Location_.span -> t) format4 -> 'a
val filename_only : string -> string -> t

val to_string : t -> string

val raise_exception : t -> _
val to_exception : ('a, t) Result.result -> 'a
val catch : (unit -> 'a) -> ('a, t) Result.result

type 'a with_warnings = {
  value : 'a;
  warnings : t list;
}

type warning_accumulator

val accumulate_warnings : (warning_accumulator -> 'a) -> 'a with_warnings
val warning : warning_accumulator -> t -> unit
val shed_warnings : 'a with_warnings -> 'a

(** When set to [true],
   [shed_warnings] will raise [Failure] if it had to print warnings. *)
val set_warn_error : bool -> unit

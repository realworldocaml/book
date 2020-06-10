open Cmdliner.Term

val named : ('a -> 'b) -> 'a t -> 'b t

val non_deterministic : [> `Non_deterministic of bool ] t

val syntax : [> `Syntax of Mdx.syntax option ] t

val file : [> `File of string ] t

val section : [> `Section of string option ] t

val silent_eval : [> `Silent_eval of bool ] t

val record_backtrace : [> `Record_backtrace of bool ] t

val silent : [> `Silent of bool ] t

val verbose_findlib : [> `Verbose_findlib of bool ] t

val prelude : [> `Prelude of string list ] t

val prelude_str : [> `Prelude_str of string list ] t

val root : [> `Root of string option ] t

val force_output : [> `Force_output of bool ] t

type output = File of string | Stdout

val output : [> `Output of output option ] t
(** A --output option to overwrite the command output.
    One can pass it ["-"] to set it to stdout which should imply [force_output].
    [default_doc] is used to describe the default value in the command's
    manpage *)

val setup : [> `Setup of unit ] t

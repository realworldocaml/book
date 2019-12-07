open! Core_kernel
open! Async_kernel
open! Import

type 'a t = 'a Or_error.t Deferred.t

include Monad.S with type 'a t := 'a t

val fail : Error.t -> _ t
val ok_unit : unit t
val ignore : _ t -> unit t
val ok_exn : 'a t -> 'a Deferred.t
val of_exn : exn -> _ t
val of_exn_result : ('a, exn) Result.t Deferred.t -> 'a t
val error : string -> 'a -> ('a -> Sexp.t) -> _ t
val error_s : Sexp.t -> _ t
val error_string : string -> _ t
val errorf : ('a, unit, string, _ t) format4 -> 'a
val tag : 'a t -> tag:string -> 'a t
val tag_arg : 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t
val unimplemented : string -> _ t
val find_map_ok : 'a list -> f:('a -> 'b t) -> 'b t

(** Note that [try_with f] is eager only in the [Ok] case. *)
val try_with
  :  ?extract_exn:bool
  -> ?here:Lexing.position
  -> ?name:string
  -> (unit -> 'a Deferred.t)
  -> 'a t

(** Note that [try_with_join f] is eager only when no exception is raised by [f]. *)
val try_with_join
  :  ?extract_exn:bool
  -> ?here:Lexing.position
  -> ?name:string
  -> (unit -> 'a t)
  -> 'a t

val combine_errors : 'a t list -> 'a list t
val combine_errors_unit : unit t list -> unit t

module List : Monad_sequence.S with type 'a monad := 'a t with type 'a t := 'a list

val repeat_until_finished
  :  'state
  -> ('state -> [`Repeat of 'state | `Finished of 'result] t)
  -> 'result t

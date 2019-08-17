open! Core
open! Import

(** Wrapper around {{!Core.User_and_group}[Core.User_and_group]} with a deferred
    [for_this_process] / [for_this_process_exn]. *)

type t = Core.User_and_group.t [@@deriving sexp, bin_io]

include Identifiable with type t := t

val create : user:string -> group:string -> t
val user : t -> string
val group : t -> string
val for_this_process : unit -> t Or_error.t Deferred.t
val for_this_process_exn : unit -> t Deferred.t

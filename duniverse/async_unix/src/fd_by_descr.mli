(** [Fd_by_descr] is a table of the open [Fd.t]s, indexed by file descriptor number.

    In this interface, we use [Raw_fd.t] rather than [Fd.t] to avoid a dependency cycle,
    because the [Fd] module can't be defined yet. *)

open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : num_file_descrs:int -> t
val capacity : t -> int
val add : t -> Raw_fd.t -> unit Or_error.t
val mem : t -> File_descr.t -> bool
val find : t -> File_descr.t -> Raw_fd.t option
val find_exn : t -> File_descr.t -> Raw_fd.t
val remove : t -> Raw_fd.t -> unit
val fold : t -> init:'a -> f:('a -> Raw_fd.t -> 'a) -> 'a
val iter : t -> f:(Raw_fd.t -> unit) -> unit

(** This library defines the command line argument -libname, shared by both ppx_bench
    and ppx_inline_test. *)
val get : unit -> (string * string) option

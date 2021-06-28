open! Core_kernel
open! Import

type t = Types.External_job.t = T : Execution_context.t * ('a -> unit) * 'a -> t
[@@deriving sexp_of]

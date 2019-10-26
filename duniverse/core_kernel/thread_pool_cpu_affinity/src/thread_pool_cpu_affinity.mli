open! Core_kernel
open! Import

module Cpuset : sig
  include Validated.S with type raw := Int.Set.t
  include Equal.S with type t := t
end

type t =
  | Inherit
  | Cpuset of Cpuset.t
[@@deriving sexp]

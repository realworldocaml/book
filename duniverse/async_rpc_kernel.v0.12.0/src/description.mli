(** Internal to [Async_rpc_kernel].  See [Rpc.Decscription]. *)

open! Core_kernel

type t =
  { name    : string
  ; version : int
  }
[@@deriving bin_io, compare, hash, sexp_of]

include Comparable.S with type t := t
include Hashable  .S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving compare, sexp, bin_io]
  end
end

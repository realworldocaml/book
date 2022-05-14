open! Core

type t =
  { prev_start : int
  ; next_start : int
  ; length : int
  }

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io]
  end
end

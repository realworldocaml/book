module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type t =
      { prev_start : int
      ; next_start : int
      ; length : int
      }
    [@@deriving sexp, bin_io]
  end
end

open! Core
include Stable.V1

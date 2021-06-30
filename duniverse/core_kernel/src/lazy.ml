open! Import

module Stable = struct
  module V1 = struct
    open Sexplib.Std

    type 'a t = 'a lazy_t [@@deriving bin_io, sexp, typerep]

    let map = Base.Lazy.map
    let compare = Base.Lazy.compare
    let t_sexp_grammar = lazy_t_sexp_grammar
  end
end

module type Base_mask = module type of Base.Lazy with type 'a t := 'a Stable.V1.t

include Stable.V1
include (Base.Lazy : Base_mask)

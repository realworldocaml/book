open! Import

type 'a t = ('a, Error.t) Result.t [@@deriving bin_io]

include (
  Base.Or_error :
    module type of struct
    include Base.Or_error
  end
  with type 'a t := 'a t)

module Stable = struct
  module V1 = struct
    type 'a t = ('a, Error.Stable.V1.t) Result.Stable.V1.t
    [@@deriving bin_io, compare, sexp]

    let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
  end

  module V2 = struct
    type 'a t = ('a, Error.Stable.V2.t) Result.Stable.V1.t
    [@@deriving bin_io, compare, sexp]

    let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
  end
end

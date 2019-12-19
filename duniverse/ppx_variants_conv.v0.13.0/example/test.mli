module Normal : sig
  type t =  Foo of int | Bar [@@deriving variants]
end

module Poly : sig
  type t =  [ `Foo of int | `Bar ] [@@deriving variants]
end

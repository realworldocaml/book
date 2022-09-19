module Normal : sig
  type t =  Foo of int | Bar [@@deriving variants]
end

module Normal_inline_record : sig
  type t =  Foo of { a : int; b : string} | Bar [@@deriving variants]
end

module Poly : sig
  type t =  [ `Foo of int | `Bar ] [@@deriving variants]
end

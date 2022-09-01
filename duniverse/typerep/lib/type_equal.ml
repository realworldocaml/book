type ('a, 'b) t = ('a, 'b) Base.Type_equal.t = T : ('a, 'a) t
type ('a, 'b) equal = ('a, 'b) t

let refl = T
let conv : type a b. (a, b) t -> a -> b = fun T x -> x

module Lift (X : sig
    type 'a t
  end) =
struct
  let lift (type a b) (T : (a, b) t) : (a X.t, b X.t) t = T
end

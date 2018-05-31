(** Core extensions. After `open Core`, can do `open Rwo_core2`
    to extend some modules in Core. *)
open Core_kernel

module Result : sig
  include module type of Result

  module List : sig
    type ('a, 'b) monad = ('a, 'b) t
    type 'a t = 'a list

    val map
      : 'a list
      -> f:('a -> ('b, 'err) monad)
      -> ('b t, 'err) monad

    val mapi
      : 'a t
      -> f:(int -> 'a -> ('b, 'err) monad)
      -> ('b t, 'err) monad

    val fold
      : 'a t
      -> init:'b
      -> f:('b -> 'a -> ('b, 'err) monad)
      -> ('b, 'err) monad

    val foldi
      : 'a t
      -> init:'b
      -> f:(int -> 'b -> 'a -> ('b, 'err) monad)
      -> ('b, 'err) monad

  end
end

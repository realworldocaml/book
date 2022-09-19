
type ('t, 'a, 'accum) fold = 't -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

type ('t, 'a, 'accum) foldi =
  't -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) -> 'accum

type ('t, 'a) iteri = 't -> f:(int -> 'a -> unit) -> unit

module type S0 = sig
  include Container.S0

  (** These are all like their equivalents in [Container] except that an index starting at
      0 is added as the first argument to [f]. *)

  val foldi : (t, elt, _) foldi
  val iteri : (t, elt) iteri
  val existsi : t -> f:(int -> elt -> bool) -> bool
  val for_alli : t -> f:(int -> elt -> bool) -> bool
  val counti : t -> f:(int -> elt -> bool) -> int
  val findi : t -> f:(int -> elt -> bool) -> (int * elt) option
  val find_mapi : t -> f:(int -> elt -> 'a option) -> 'a option
end

module type S1 = sig
  include Container.S1

  (** These are all like their equivalents in [Container] except that an index starting at
      0 is added as the first argument to [f]. *)

  val foldi : ('a t, 'a, _) foldi
  val iteri : ('a t, 'a) iteri
  val existsi : 'a t -> f:(int -> 'a -> bool) -> bool
  val for_alli : 'a t -> f:(int -> 'a -> bool) -> bool
  val counti : 'a t -> f:(int -> 'a -> bool) -> int
  val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
  val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option
end

module type Generic = sig
  include Container.Generic

  (** These are all like their equivalents in [Container] except that an index starting at
      0 is added as the first argument to [f]. *)

  val foldi : ('a t, 'a elt, _) foldi
  val iteri : ('a t, 'a elt) iteri
  val existsi : 'a t -> f:(int -> 'a elt -> bool) -> bool
  val for_alli : 'a t -> f:(int -> 'a elt -> bool) -> bool
  val counti : 'a t -> f:(int -> 'a elt -> bool) -> int
  val findi : 'a t -> f:(int -> 'a elt -> bool) -> (int * 'a elt) option
  val find_mapi : 'a t -> f:(int -> 'a elt -> 'b option) -> 'b option
end

module type Make_gen_arg = sig
  include Container_intf.Make_gen_arg

  val iteri : [ `Define_using_fold | `Custom of ('a t, 'a elt) iteri ]
  val foldi : [ `Define_using_fold | `Custom of ('a t, 'a elt, _) foldi ]
end

module type Make_arg = Make_gen_arg with type 'a elt := 'a Monad.Ident.t

module type Make0_arg = sig
  include Container_intf.Make0_arg
  include Make_gen_arg with type 'a t := t and type 'a elt := Elt.t
end

module type Indexed_container = sig
  (** Provides generic signatures for containers that support indexed iteration ([iteri],
      [foldi], ...). In principle, any container that has [iter] can also implement [iteri],
      but the idea is that [Indexed_container_intf] should be included only for containers
      that have a meaningful underlying ordering. *)

  module type Generic = Generic
  module type S0 = S0
  module type S1 = S1

  (** Generic definitions of [foldi] and [iteri] in terms of [fold].

      E.g., [iteri ~fold t ~f = ignore (fold t ~init:0 ~f:(fun i x -> f i x; i + 1))]. *)

  val foldi : fold:('t, 'a, 'accum) fold -> ('t, 'a, 'accum) foldi
  val iteri : fold:('t, 'a, int) fold -> ('t, 'a) iteri

  (** Generic definitions of indexed container operations in terms of [foldi]. *)

  val counti : foldi:('t, 'a, int) foldi -> 't -> f:(int -> 'a -> bool) -> int

  (** Generic definitions of indexed container operations in terms of [iteri]. *)

  val existsi : iteri:('t, 'a) iteri -> 't -> f:(int -> 'a -> bool) -> bool
  val for_alli : iteri:('t, 'a) iteri -> 't -> f:(int -> 'a -> bool) -> bool
  val findi : iteri:('t, 'a) iteri -> 't -> f:(int -> 'a -> bool) -> (int * 'a) option
  val find_mapi : iteri:('t, 'a) iteri -> 't -> f:(int -> 'a -> 'b option) -> 'b option

  module Make (T : Make_arg) : S1 with type 'a t := 'a T.t
  module Make0 (T : Make0_arg) : S0 with type t := T.t and type elt := T.Elt.t

  module Make_gen (T : Make_gen_arg) :
    Generic with type 'a t := 'a T.t and type 'a elt := 'a T.elt
end

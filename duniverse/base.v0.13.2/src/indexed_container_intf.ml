
type ('t, 'a, 'accum) fold = 't -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

type ('t, 'a, 'accum) foldi =
  't -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) -> 'accum

type ('t, 'a) iteri = 't -> f:(int -> 'a -> unit) -> unit

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

module type Make_arg = sig
  include Container_intf.Make_arg

  val iteri : [ `Define_using_fold | `Custom of ('a t, 'a) iteri ]
  val foldi : [ `Define_using_fold | `Custom of ('a t, 'a, _) foldi ]
end

module type Indexed_container = sig
  (** Provides generic signatures for containers that support indexed iteration ([iteri],
      [foldi], ...). In principle, any container that has [iter] can also implement [iteri],
      but the idea is that [Indexed_container_intf] should be included only for containers
      that have a meaningful underlying ordering. *)

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
end

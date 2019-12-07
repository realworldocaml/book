open! Import

include Binary_searchable_intf

module type Arg = sig
  type 'a elt
  type 'a t
  val get : 'a t -> int -> 'a elt
  val length : _ t -> int
end

module Make_gen (T : Arg) = struct
  let get = T.get
  let length = T.length

  let binary_search ?pos ?len t ~compare how v =
    Binary_search.binary_search ?pos ?len t ~get ~length ~compare how v

  let binary_search_segmented ?pos ?len t ~segment_of how =
    Binary_search.binary_search_segmented ?pos ?len t ~get ~length ~segment_of how
end

module Make (T : Indexable) =
  Make_gen (struct
    type 'a elt = T.elt
    type 'a t   = T.t
    include (T : Indexable with type elt := T.elt with type t := T.t)
  end)

module Make1 (T : Indexable1) =
  Make_gen (struct
    type 'a elt = 'a
    type 'a t = 'a T.t
    let get = T.get
    let length = T.length
  end)

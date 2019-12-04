(** General functions for performing binary searches over ordered sequences given
    [length] and [get] functions.

    These functions can be specialized and added to a data structure using the functors
    supplied in {{!Base.Binary_searchable}[Binary_searchable]} and described in
    {{!Base.Binary_searchable_intf}[Binary_searchable_intf]}.

    {2:examples Examples}

    Below we assume that the functions [get], [length] and [compare] are in scope:

    {[
      (* Find the index of an element [e] in [t] *)
      binary_search t ~get ~length ~compare `First_equal_to e;

      (* Find the index where an element [e] should be inserted *)
      binary_search t ~get ~length ~compare `First_greater_than_or_equal_to e;

      (* Find the index in [t] where all elements to the left are less than [e] *)
      binary_search_segmented t ~get ~length ~segment_of:(fun e' ->
        if compare e' e <= 0 then `Left else `Right) `First_on_right
    ]} *)

open! Import

(** [binary_search ?pos ?len t ~length ~get ~compare which elt] takes [t] that is sorted
    in increasing order according to [compare], where [compare] and [elt] divide [t] into
    three (possibly empty) segments:

    {v
      |  < elt  |  = elt  |  > elt  |
    v}

    [binary_search] returns the index in [t] of an element on the boundary of segments
    as specified by [which].  See the diagram below next to the [which] variants.

    By default, [binary_search] searches the entire [t].  One can supply [?pos] or
    [?len] to search a slice of [t].

    [binary_search] does not check that [compare] orders [t], and behavior is
    unspecified if [compare] doesn't order [t].  Behavior is also unspecified if
    [compare] mutates [t]. *)
val binary_search
  :  ?pos:int
  -> ?len:int
  -> 't
  -> length:('t -> int)
  -> get:('t -> int -> 'elt)
  -> compare:('elt -> 'key -> int)
  -> [ `Last_strictly_less_than         (** {v | < elt X |                       v} *)
     | `Last_less_than_or_equal_to      (** {v |      <= elt       X |           v} *)
     | `Last_equal_to                   (** {v           |   = elt X |           v} *)
     | `First_equal_to                  (** {v           | X = elt   |           v} *)
     | `First_greater_than_or_equal_to  (** {v           | X       >= elt      | v} *)
     | `First_strictly_greater_than     (** {v                       | X > elt | v} *)
     ]
  -> 'key
  -> int option

(** [binary_search_segmented ?pos ?len t ~length ~get ~segment_of which] takes a
    [segment_of] function that divides [t] into two (possibly empty) segments:

    {v
      | segment_of elt = `Left | segment_of elt = `Right |
    v}

    [binary_search_segmented] returns the index of the element on the boundary of the
    segments as specified by [which]: [`Last_on_left] yields the index of the last
    element of the left segment, while [`First_on_right] yields the index of the first
    element of the right segment.  It returns [None] if the segment is empty.

    By default, [binary_search] searches the entire [t].  One can supply [?pos] or
    [?len] to search a slice of [t].

    [binary_search_segmented] does not check that [segment_of] segments [t] as in the
    diagram, and behavior is unspecified if [segment_of] doesn't segment [t].  Behavior
    is also unspecified if [segment_of] mutates [t]. *)
val binary_search_segmented
  :  ?pos:int
  -> ?len:int
  -> 't
  -> length:('t -> int)
  -> get:('t -> int -> 'elt)
  -> segment_of:('elt -> [ `Left | `Right ])
  -> [ `Last_on_left | `First_on_right ]
  -> int option

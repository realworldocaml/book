(* Be sure and first read the implementation overview in timing_wheel_intf.ml.

   A timing wheel is represented as an array of "levels", where each level is an array of
   "slots".  Each slot represents a range of keys, and holds elements associated with
   those keys.  Each level is determined by two parameters: [bits], the number of key bits
   that that level is responsible for distinguishing, and [bits_per_slot], the size of the
   range of keys that correspond to a single slot in the array.  Conceptually, each level
   breaks up all possible keys into ranges of size [2^bits_per_slot].  The length of a
   level array is [2^bits], and the array is used like a circular buffer to traverse the
   ranges as the timing wheel's [min_allowed_key] increases.  A key [k], if stored in the
   level, is stored at index [(k / 2^bits_per_slot) mod 2^bits].

   The settings of the [bits] values are configurable by user code using [Level_bits],
   although there is a reasonable default setting.  Given the [bits] values, the
   [bits_per_slot] are chosen so that [bits_per_slot] at level [i] is the sum of the
   [bits] at all lower levels.  Thus, a slot's range at level [i] is as large as the
   entire range of the array at level [i - 1].

   Each level has a [min_allowed_key] and a [max_allowed_key] that determine the range of
   keys that it currently represents.  The crucial invariant of the timing wheel data
   structure is that the [min_allowed_key] at level [i] is no more than the
   [max_allowed_key + 1] of level [i - 1].  This ensures that the levels can represent all
   keys from the [min_allowed_key] of the lowest level to the [max_allowed_key] of the
   highest level.  The [increase_min_allowed_key] function is responsible for restoring
   this invariant.

   At level 0, [bits_per_slot = 0], and so the size of each slot is [1].  That is, level 0
   precisely distinguishes all the keys between its [min_allowed_key] (which is the same
   as the [min_allowed_key] of the entire timing wheel) and [max_allowed_key].  As the
   levels increase, the [min_allowed_key] increases, the [bits_per_slot] increases, and
   the range of keys stored in the level increases (dramatically).

   The idea of the implementation is similar to the hierarchical approach described in:

   {v
     Hashed and Hierarchical Timing Wheels:
     Efficient Data Structures for Implementing a Timer Facility

     Varghese & Lauck, 1996
   v}

   However, the code is completely new. *)

open! Core_kernel
open! Import
open! Timing_wheel_intf
module Pool = Tuple_pool
module Time_ns = Core_kernel_private.Time_ns_alternate_sexp

let sexp_of_t_style : [ `Pretty | `Internal ] ref = ref `Pretty

(* [{max,min}_time] are bounds on the times supported by a timing wheel. *)

let max_time = Time_ns.max_value_representable
let min_time = Time_ns.epoch

module Num_key_bits : sig
  type t = private int [@@deriving compare, sexp]

  include Comparable with type t := t
  include Invariant.S with type t := t

  val zero : t

  (* val min_value : t *)

  val max_value : t
  val to_int : t -> int
  val of_int : int -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val pow2 : t -> Int63.t
end = struct
  include Int

  let min_value = 0

  (** We support all non-negative [Time_ns.t] values. *)
  let max_value = Int63.num_bits - 1

  let invariant t =
    assert (t >= min_value);
    assert (t <= max_value)
  ;;

  let of_int i =
    invariant i;
    i
  ;;

  let ( + ) t1 t2 =
    let t = t1 + t2 in
    invariant t;
    t
  ;;

  let ( - ) t1 t2 =
    let t = t1 - t2 in
    invariant t;
    t
  ;;

  let pow2 t = Int63.shift_left Int63.one t
end

module Level_bits = struct
  type t = Num_key_bits.t list [@@deriving compare, sexp]

  let max_num_bits = (Num_key_bits.max_value :> int)
  let num_bits_internal t = List.fold t ~init:Num_key_bits.zero ~f:Num_key_bits.( + )
  let num_bits t = (num_bits_internal t :> int)

  let invariant t =
    assert (not (List.is_empty t));
    List.iter t ~f:(fun num_key_bits ->
      Num_key_bits.invariant num_key_bits;
      assert (Num_key_bits.( > ) num_key_bits Num_key_bits.zero));
    Num_key_bits.invariant (num_bits_internal t)
  ;;

  let t_of_sexp sexp =
    let t = sexp |> [%of_sexp: t] in
    invariant t;
    t
  ;;

  let create_exn ?(extend_to_max_num_bits = false) ints =
    if List.is_empty ints then failwith "Level_bits.create_exn requires a nonempty list";
    if List.exists ints ~f:(fun bits -> bits <= 0)
    then
      raise_s
        [%message "Level_bits.create_exn got nonpositive num bits" ~_:(ints : int list)];
    let num_bits = List.fold ints ~init:0 ~f:( + ) in
    if num_bits > max_num_bits
    then
      raise_s
        [%message
          "Level_bits.create_exn got too many bits"
            ~_:(ints : int list)
            ~got:(num_bits : int)
            (max_num_bits : int)];
    let ints =
      if extend_to_max_num_bits
      then ints @ List.init (max_num_bits - num_bits) ~f:(const 1)
      else ints
    in
    List.map ints ~f:Num_key_bits.of_int
  ;;

  let default = create_exn [ 11; 10; 10; 10; 10; 10; 1 ]

  let trim t ~max_num_bits =
    if Num_key_bits.( <= ) (num_bits_internal t) max_num_bits
    then t
    else (
      let rec loop t ~remaining =
        match t with
        | [] -> []
        | b :: t ->
          if Num_key_bits.( >= ) b remaining
          then [ remaining ]
          else b :: loop t ~remaining:(Num_key_bits.( - ) remaining b)
      in
      loop t ~remaining:max_num_bits)
  ;;
end

module Alarm_precision : sig
  include Alarm_precision

  val num_key_bits : t -> Num_key_bits.t
  val interval_num : t -> Time_ns.t -> Int63.t
  val interval_num_start : t -> Int63.t -> Time_ns.t
end = struct
  (** [t] is represented as the log2 of a number of nanoseconds. *)
  type t = int [@@deriving compare, hash]

  let equal = [%compare.equal: t]
  let num_key_bits t = t |> Num_key_bits.of_int

  let to_span t =
    if t < 0
    then
      raise_s
        [%message
          "[Alarm_precision.to_span] of negative power of two nanoseconds" ~_:(t : int)];
    Int63.(shift_left one) t |> Time_ns.Span.of_int63_ns
  ;;

  let sexp_of_t t = [%sexp (t |> to_span : Time_ns.Span.t)]
  let one_nanosecond = 0
  let about_one_microsecond = 10
  let about_one_millisecond = 20
  let about_one_second = 30
  let about_one_day = 46
  let mul t ~pow2 = t + pow2
  let div t ~pow2 = t - pow2
  let interval_num t time = Int63.shift_right (time |> Time_ns.to_int63_ns_since_epoch) t

  let interval_num_start t interval_num =
    Int63.shift_left interval_num t |> Time_ns.of_int63_ns_since_epoch
  ;;

  let of_span_floor_pow2_ns span =
    if Time_ns.Span.( <= ) span Time_ns.Span.zero
    then
      raise_s
        [%message
          "[Alarm_precision.of_span_floor_pow2_ns] got non-positive span"
            (span : Time_ns.Span.t)];
    span |> Time_ns.Span.to_int63_ns |> Int63.floor_log2
  ;;

  let of_span = of_span_floor_pow2_ns

  module Unstable = struct
    module T = struct
      type nonrec t = t [@@deriving compare]

      let of_binable = of_span_floor_pow2_ns
      let to_binable = to_span
      let of_sexpable = of_span_floor_pow2_ns
      let to_sexpable = to_span
    end

    include T
    include Binable.Of_binable_without_uuid [@alert "-legacy"] (Time_ns.Span) (T)
    include Sexpable.Of_sexpable (Time_ns.Span) (T)
  end
end

module Config = struct
  let level_bits_default = Level_bits.default

  type t =
    { alarm_precision : Alarm_precision.Unstable.t
    ; level_bits : Level_bits.t [@default level_bits_default]
    ; capacity : int option [@sexp.option]
    }
  [@@deriving fields, sexp]

  let alarm_precision t = Alarm_precision.to_span t.alarm_precision

  (* [max_num_level_bits alarm_precision] returns the number of level bits needed for a
     timing wheel with the specified [alarm_precision] to be able to represent all
     possible times from [Time_ns.epoch] onward.  Since non-negative times have 62 bits,
     we require [L <= 62 - A], where [A] is the number of alarm bits and [L] is the
     number of level bits. *)
  let max_num_level_bits alarm_precision =
    Num_key_bits.( - )
      Num_key_bits.max_value
      (Alarm_precision.num_key_bits alarm_precision)
  ;;

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      assert (
        Num_key_bits.( <= )
          (Level_bits.num_bits_internal t.level_bits)
          (max_num_level_bits t.alarm_precision));
      let check f = Invariant.check_field t f in
      Fields.iter
        ~alarm_precision:ignore
        ~capacity:ignore
        ~level_bits:(check Level_bits.invariant))
  ;;

  let create ?capacity ?(level_bits = level_bits_default) ~alarm_precision () =
    let level_bits =
      Level_bits.trim level_bits ~max_num_bits:(max_num_level_bits alarm_precision)
    in
    { alarm_precision; level_bits; capacity }
  ;;

  let microsecond_precision () =
    create
      ()
      ~alarm_precision:Alarm_precision.about_one_microsecond
      ~level_bits:(Level_bits.create_exn [ 10; 10; 6; 6; 5 ])
  ;;

  let durations t =
    List.folding_map
      t.level_bits
      ~init:(Alarm_precision.num_key_bits t.alarm_precision |> Num_key_bits.to_int)
      ~f:(fun num_bits_accum level_num_bits ->
        let num_bits_accum = num_bits_accum + (level_num_bits |> Num_key_bits.to_int) in
        let duration =
          Time_ns.Span.of_int63_ns
            (if num_bits_accum = Int63.num_bits - 1
             then Int63.max_value
             else Int63.shift_left Int63.one num_bits_accum)
        in
        num_bits_accum, duration)
  ;;
end


(** Timing wheel is implemented as a priority queue in which the keys are
    non-negative integers corresponding to the intervals of time.  The priority queue is
    unlike a typical priority queue in that rather than having a "delete min" operation,
    it has a nondecreasing minimum allowed key, which corresponds to the current time,
    and an [increase_min_allowed_key] operation, which implements [advance_clock].
    [increase_min_allowed_key] as a side effect removes all elements from the timing
    wheel whose key is smaller than the new minimum, which implements firing the alarms
    whose time has expired.

    Adding elements to and removing elements from a timing wheel takes constant time,
    unlike a heap-based priority queue which takes log(N), where N is the number of
    elements in the heap.  [increase_min_allowed_key] takes time proportional to the
    amount of increase in the min-allowed key, as compared to log(N) for a heap.  It is
    these performance differences that motivate the existence of timing wheels and make
    them a good choice for maintaing a set of alarms.  With a timing wheel, one can
    support any number of alarms paying constant overhead per alarm, while paying a
    small constant overhead per unit of time passed.

    As the minimum allowed key increases, the timing wheel does a lazy radix sort of the
    element keys, with level 0 handling the least significant [b_0] bits in a key, and
    each subsequent level [i] handling the next most significant [b_i] bits.  The levels
    hold increasingly larger ranges of keys, where the union of all the levels can hold
    any key from [min_allowed_key t] to [max_allowed_key t].  When a key is added to the
    timing wheel, it is added at the lowest possible level that can store the key.  As
    the minimum allowed key increases, timing-wheel elements move down levels until they
    reach level 0, and then are eventually removed.  *)
module Priority_queue : sig
  type 'a t [@@deriving sexp_of]
  type 'a priority_queue = 'a t

  module Key : Interval_num

  module Elt : sig
    (** An [Elt.t] represents an element that was added to a timing wheel. *)
    type 'a t [@@deriving sexp_of]

    val at : 'a priority_queue -> 'a t -> Time_ns.t
    val key : 'a priority_queue -> 'a t -> Key.t
    val value : 'a priority_queue -> 'a t -> 'a
    val null : unit -> 'a t
  end

  module Internal_elt : sig
    module Pool : sig
      type 'a t
    end

    type 'a t

    val key : 'a Pool.t -> 'a t -> Key.t
    val max_alarm_time : 'a Pool.t -> 'a t -> with_key:Key.t -> Time_ns.t
    val is_null : _ t -> bool
    val to_external : 'a t -> 'a Elt.t
  end

  val pool : 'a t -> 'a Internal_elt.Pool.t

  include Invariant.S1 with type 'a t := 'a t

  (** [create ?level_bits ()] creates a new empty timing wheel, [t], with [length t = 0]
      and [min_allowed_key t = 0]. *)
  val create : ?capacity:int -> ?level_bits:Level_bits.t -> unit -> 'a t

  (** [length t] returns the number of elements in the timing wheel. *)
  val length : _ t -> int

  (** [min_allowed_key t] is the minimum key that can be stored in [t].  This only
      indicates the possibility; there need not be an element [elt] in [t] with [Elt.key
      elt = min_allowed_key t].  This is not the same as the "min_key" operation in a
      typical priority queue.

      [min_allowed_key t] can increase over time, via calls to
      [increase_min_allowed_key]. *)
  val min_allowed_key : _ t -> Key.t

  (** [max_allowed_key t] is the maximum allowed key that can be stored in [t].  As
      [min_allowed_key] increases, so does [max_allowed_key]; however it is not the case
      that [max_allowed_key t - min_allowed_key t] is a constant.  It is guaranteed that
      [max_allowed_key t >= min_allowed_key t + 2^B - 1],
      where [B] is the sum of the b_i in [level_bits]. *)
  val max_allowed_key : _ t -> Key.t

  val min_elt_ : 'a t -> 'a Internal_elt.t
  val internal_add : 'a t -> key:Key.t -> at:Time_ns.t -> 'a -> 'a Internal_elt.t

  (** [remove t elt] removes [elt] from [t].  It is an error if [elt] is not currently
      in [t], and this error may or may not be detected. *)
  val remove : 'a t -> 'a Elt.t -> unit

  val change : 'a t -> 'a Elt.t -> key:Key.t -> at:Time_ns.t -> unit

  (** [clear t] removes all elts from [t]. *)
  val clear : _ t -> unit

  val mem : 'a t -> 'a Elt.t -> bool

  (** [increase_min_allowed_key t ~key ~handle_removed] increases the minimum allowed
      key in [t] to [key], and removes all elements with keys less than [key], applying
      [handle_removed] to each element that is removed.  If [key <= min_allowed_key t],
      then [increase_min_allowed_key] does nothing.  Otherwise, if
      [increase_min_allowed_key] returns successfully, [min_allowed_key t = key].

      [increase_min_allowed_key] takes time proportional to [key - min_allowed_key t],
      although possibly less time.

      Behavior is unspecified if [handle_removed] accesses [t] in any way other than
      [Elt] functions. *)
  val increase_min_allowed_key
    :  'a t
    -> key:Key.t
    -> handle_removed:('a Elt.t -> unit)
    -> unit

  val iter : 'a t -> f:('a Elt.t -> unit) -> unit

  val fire_past_alarms
    :  'a t
    -> handle_fired:('a Elt.t -> unit)
    -> key:Key.t
    -> now:Time_ns.t
    -> unit
end = struct
  (** Each slot in a level is a (possibly null) pointer to a circular doubly-linked list
      of elements.  We pool the elements so that we can reuse them after they are removed
      from the timing wheel (either via [remove] or [increase_min_allowed_key]).  In
      addition to storing the [key], [at], and [value] in the element, we store the
      [level_index] so that we can quickly get to the level holding an element when we
      [remove] it.

      We distinguish between [External_elt] and [Internal_elt], which are the same
      underneath.  We maintain the invariant that an [Internal_elt] is either [null] or a
      valid pointer.  On the other hand, [External_elt]s are returned to user code, so
      there is no guarantee of validity -- we always validate an [External_elt] before
      doing anything with it.

      It is therefore OK to use [Pool.Unsafe], because we will never attempt to access a
      slot of an invalid pointer. *)
  module Pool = Pool.Unsafe

  module Pointer = Pool.Pointer

  module Key : sig
    (** [Interval_num] is the public API.  Everything following in the signature is
        for internal use. *)
    include
      Timing_wheel_intf.Interval_num

    (** [add_clamp_to_max] doesn't work at all with negative spans *)
    val add_clamp_to_max : t -> Span.t -> t

    val succ_clamp_to_max : t -> t

    (** [Slots_mask] is used to quickly determine a key's slot in a given level. *)
    module Slots_mask : sig
      type t = private Int63.t [@@deriving compare, sexp_of]

      val create : level_bits:Num_key_bits.t -> t
      val next_slot : t -> int -> int
    end

    (** [Min_key_in_same_slot_mask] is used to quickly determine the minimum key in the
        same slot as a given key. *)
    module Min_key_in_same_slot_mask : sig
      type t = private Int63.t [@@deriving compare, sexp_of]

      include Equal.S with type t := t

      val create : bits_per_slot:Num_key_bits.t -> t
    end

    val num_keys : Num_key_bits.t -> Span.t
    val min_key_in_same_slot : t -> Min_key_in_same_slot_mask.t -> t
    val slot : t -> bits_per_slot:Num_key_bits.t -> slots_mask:Slots_mask.t -> int
  end = struct
    module Slots_mask = struct
      type t = Int63.t [@@deriving compare, sexp_of]

      let create ~level_bits = Int63.( - ) (Num_key_bits.pow2 level_bits) Int63.one
      let next_slot t slot = (slot + 1) land Int63.to_int_exn t
    end

    let num_keys num_bits = Num_key_bits.pow2 num_bits

    module Min_key_in_same_slot_mask = struct
      include Int63

      let create ~bits_per_slot = bit_not (Num_key_bits.pow2 bits_per_slot - one)
    end

    module Span = struct
      include Int63

      let to_int63 t = t
      let of_int63 i = i
      let scale_int t i = t * of_int i
    end

    include Int63

    let of_int63 i = i
    let to_int63 t = t
    let add t i = t + i
    let add_clamp_to_max t i = if t > max_value - i then max_value else t + i
    let succ_clamp_to_max t = if t = max_value then max_value else succ t
    let sub t i = t - i
    let diff t1 t2 = t1 - t2

    let slot t ~(bits_per_slot : Num_key_bits.t) ~slots_mask =
      to_int_exn (bit_and (shift_right t (bits_per_slot :> int)) slots_mask)
    ;;

    let min_key_in_same_slot t min_key_in_same_slot_mask =
      bit_and t min_key_in_same_slot_mask
    ;;
  end

  module Min_key_in_same_slot_mask = Key.Min_key_in_same_slot_mask
  module Slots_mask = Key.Slots_mask

  module External_elt = struct

    (** The [pool_slots] here has nothing to do with the slots in a level array.  This is
        for the slots in the pool tuple representing a level element. *)
    type 'a pool_slots =
      ( Key.t
      , Time_ns.t
      , 'a
      , int
      , 'a pool_slots Pointer.t
      , 'a pool_slots Pointer.t )
        Pool.Slots.t6
    [@@deriving sexp_of]

    type 'a t = 'a pool_slots Pointer.t [@@deriving sexp_of]

    let null = Pointer.null
  end

  module Internal_elt : sig
    module Pool : sig
      type 'a t [@@deriving sexp_of]

      include Invariant.S1 with type 'a t := 'a t

      val create : ?capacity:int -> unit -> _ t
      val is_full : _ t -> bool
      val grow : ?capacity:int -> 'a t -> 'a t
    end

    type 'a t = private 'a External_elt.t [@@deriving sexp_of]

    val null : unit -> _ t
    val is_null : _ t -> bool
    val is_valid : 'a Pool.t -> 'a t -> bool

    (** Dealing with [External_elt]s. *)

    val external_is_valid : 'a Pool.t -> 'a External_elt.t -> bool
    val to_external : 'a t -> 'a External_elt.t
    val of_external_exn : 'a Pool.t -> 'a External_elt.t -> 'a t
    val equal : 'a t -> 'a t -> bool
    val invariant : 'a Pool.t -> ('a -> unit) -> 'a t -> unit

    (** [create] returns an element whose [next] and [prev] are [null]. *)
    val create
      :  'a Pool.t
      -> key:Key.t
      (** [at] is used when the priority queue is used to implement a timing wheel.  If
          unused, it will be [Time_ns.epoch]. *)
      -> at:Time_ns.t
      -> value:'a
      -> level_index:int
      -> 'a t

    val free : 'a Pool.t -> 'a t -> unit

    (** accessors *)

    val key : 'a Pool.t -> 'a t -> Key.t
    val at : 'a Pool.t -> 'a t -> Time_ns.t
    val level_index : 'a Pool.t -> 'a t -> int
    val next : 'a Pool.t -> 'a t -> 'a t
    val value : 'a Pool.t -> 'a t -> 'a

    (** mutators *)

    val set_key : 'a Pool.t -> 'a t -> Key.t -> unit
    val set_at : 'a Pool.t -> 'a t -> Time_ns.t -> unit
    val set_level_index : 'a Pool.t -> 'a t -> int -> unit

    (** [insert_at_end pool t ~to_add] treats [t] as the head of the list and adds [to_add]
        to the end of it. *)
    val insert_at_end : 'a Pool.t -> 'a t -> to_add:'a t -> unit

    (** [link_to_self pool t] makes [t] be a singleton circular doubly-linked list. *)
    val link_to_self : 'a Pool.t -> 'a t -> unit

    (** [unlink p t] unlinks [t] from the circularly doubly-linked list that it is in.  It
        changes the pointers of [t]'s [prev] and [next] elts, but not [t]'s [prev] and
        [next] pointers.  [unlink] is meaningless if [t] is a singleton. *)
    val unlink : 'a Pool.t -> 'a t -> unit

    (** Iterators.  [iter p t ~init ~f] visits each element in the doubly-linked list
        containing [t], starting at [t], and following [next] pointers.  [length] counts
        by visiting each element in the list. *)
    val iter : 'a Pool.t -> 'a t -> f:('a t -> unit) -> unit

    val length : 'a Pool.t -> 'a t -> int

    (** [max_alarm_time t elt ~with_key] finds the max [at] in [elt]'s list among the elts
        whose key is [with_key], returning [Time_ns.epoch] if the list is empty. *)
    val max_alarm_time : 'a Pool.t -> 'a t -> with_key:Key.t -> Time_ns.t
  end = struct
    type 'a pool_slots = 'a External_elt.pool_slots [@@deriving sexp_of]
    type 'a t = 'a External_elt.t [@@deriving sexp_of]

    let null = Pointer.null
    let is_null = Pointer.is_null
    let equal t1 t2 = Pointer.phys_equal t1 t2

    let create pool ~key ~at ~value ~level_index =
      Pool.new6 pool key at value level_index (null ()) (null ())
    ;;

    let free = Pool.free
    let key p t = Pool.get p t Pool.Slot.t0
    let set_key p t k = Pool.set p t Pool.Slot.t0 k
    let at p t = Pool.get p t Pool.Slot.t1
    let set_at p t x = Pool.set p t Pool.Slot.t1 x
    let value p t = Pool.get p t Pool.Slot.t2
    let level_index p t = Pool.get p t Pool.Slot.t3
    let set_level_index p t i = Pool.set p t Pool.Slot.t3 i
    let prev p t = Pool.get p t Pool.Slot.t4
    let set_prev p t x = Pool.set p t Pool.Slot.t4 x
    let next p t = Pool.get p t Pool.Slot.t5
    let set_next p t x = Pool.set p t Pool.Slot.t5 x
    let is_valid p t = Pool.pointer_is_valid p t
    let external_is_valid = is_valid

    let invariant pool invariant_a t =
      Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
        assert (is_valid pool t);
        invariant_a (value pool t);
        let n = next pool t in
        assert (is_null n || Pointer.phys_equal t (prev pool n));
        let p = prev pool t in
        assert (is_null p || Pointer.phys_equal t (next pool p)))
    ;;

    module Pool = struct
      type 'a t = 'a pool_slots Pool.t [@@deriving sexp_of]

      let invariant _invariant_a t = Pool.invariant ignore t
      let create ?(capacity = 1) () = Pool.create Pool.Slots.t6 ~capacity
      let grow = Pool.grow
      let is_full = Pool.is_full
    end

    let to_external t = t

    let of_external_exn pool t =
      if is_valid pool t then t else raise_s [%message "Timing_wheel got invalid alarm"]
    ;;

    let unlink pool t =
      set_next pool (prev pool t) (next pool t);
      set_prev pool (next pool t) (prev pool t)
    ;;

    let link pool prev next =
      set_next pool prev next;
      set_prev pool next prev
    ;;

    let link_to_self pool t = link pool t t

    let insert_at_end pool t ~to_add =
      let prev = prev pool t in
      link pool prev to_add;
      link pool to_add t
    ;;

    let iter pool first ~f =
      let current = ref first in
      let continue = ref true in
      while !continue do
        (* We get [next] before calling [f] so that [f] can modify or [free] [!current]. *)
        let next = next pool !current in
        f !current;
        if phys_equal next first then continue := false else current := next
      done
    ;;

    let length pool first =
      let r = ref 0 in
      let current = ref first in
      let continue = ref true in
      while !continue do
        incr r;
        let next = next pool !current in
        if phys_equal next first then continue := false else current := next
      done;
      !r
    ;;

    let max_alarm_time pool first ~with_key =
      let max_alarm_time = ref Time_ns.epoch in
      let current = ref first in
      let continue = ref true in
      while !continue do
        let next = next pool !current in
        if Key.equal (key pool !current) with_key
        then max_alarm_time := Time_ns.max (at pool !current) !max_alarm_time;
        if phys_equal next first then continue := false else current := next
      done;
      !max_alarm_time
    ;;
  end

  module Level = struct
    (** For given level, one can break the bits into a key into three regions:

        {v
         | higher levels | this level | lower levels |
        v}

        "Lower levels" is [bits_per_slot] bits wide.  "This level" is [bits] wide. *)
    type 'a t =
      { (* The [index] in the timing wheel's array of levels where this level is. *)
        index : int
      ; (* How many [bits] this level is responsible for. *)
        bits : Num_key_bits.t
      ; (* [slots_mask = Slots_mask.create ~level_bits:t.bits]. *)
        slots_mask : Slots_mask.t
      ; (* [bits_per_slot] is how many bits each slot distinguishes, and is the sum of of
           the [bits] of all the lower levels. *)
        bits_per_slot : Num_key_bits.t
      ; keys_per_slot : Key.Span.t
      ; min_key_in_same_slot_mask : Min_key_in_same_slot_mask.t
      ; (* [diff_max_min_allowed_key = keys_per_slot * Array.length slots - 1] *)
        diff_max_min_allowed_key : Key.Span.t
      ; (* [length] is the number of elts currently in this level. *)
        mutable length : int
      ; (* All elements at this level have their [key] satisfy [min_allowed_key <= key <=
           max_allowed_key].  Also, [min_allowed_key] is a multiple of [keys_per_slot]. *)
        mutable min_allowed_key : Key.t
      ; mutable max_allowed_key : Key.t
      ; (* [slots] holds the (possibly null) pointers to the circular doubly-linked lists
           of elts.  [Array.length slots = 1 lsl bits]. *)
        slots : ('a Internal_elt.t array[@sexp.opaque])
      }
    [@@deriving fields, sexp_of]

    let slot t ~key =
      Key.slot key ~bits_per_slot:t.bits_per_slot ~slots_mask:t.slots_mask
    ;;

    let next_slot t slot = Slots_mask.next_slot t.slots_mask slot

    let min_key_in_same_slot t ~key =
      Key.min_key_in_same_slot key t.min_key_in_same_slot_mask
    ;;

    let compute_min_allowed_key t ~prev_level_max_allowed_key =
      (* This computation ensures that [t]'s [min_allowed_key] is as large as possible
         subject to the constraint that there is no inter-level gap. *)
      if Key.equal prev_level_max_allowed_key Key.max_value
      then Key.max_value
      else min_key_in_same_slot t ~key:(Key.succ prev_level_max_allowed_key)
    ;;
  end

  type 'a t =
    { mutable length : int
    ; mutable pool : 'a Internal_elt.Pool.t
    ; (* [min_elt] is either null or an element whose key is [elt_key_lower_bound]. *)
      mutable min_elt : 'a Internal_elt.t
    ; (* All elements in the priority queue have their key [>= elt_key_lower_bound]. *)
      mutable elt_key_lower_bound : Key.t
    ; levels : 'a Level.t array
    }
  [@@deriving fields, sexp_of]

  type 'a priority_queue = 'a t

  module Elt = struct
    type 'a t = 'a External_elt.t [@@deriving sexp_of]

    let null = External_elt.null
    let at p t = Internal_elt.at p.pool (Internal_elt.of_external_exn p.pool t)
    let key p t = Internal_elt.key p.pool (Internal_elt.of_external_exn p.pool t)
    let value p t = Internal_elt.value p.pool (Internal_elt.of_external_exn p.pool t)
  end

  let sexp_of_t_internal = sexp_of_t
  let is_empty t = length t = 0
  let num_levels t = Array.length t.levels
  let min_allowed_key t = Level.min_allowed_key t.levels.(0)
  let max_allowed_key t = Level.max_allowed_key t.levels.(num_levels t - 1)

  let internal_iter t ~f =
    if t.length > 0
    then (
      let pool = t.pool in
      let levels = t.levels in
      for level_index = 0 to Array.length levels - 1 do
        let level = levels.(level_index) in
        if level.length > 0
        then (
          let slots = level.slots in
          for slot_index = 0 to Array.length slots - 1 do
            let elt = slots.(slot_index) in
            if not (Internal_elt.is_null elt) then Internal_elt.iter pool elt ~f
          done)
      done)
  ;;

  let iter t ~f = internal_iter t ~f:(f : _ Elt.t -> unit :> _ Internal_elt.t -> unit)

  module Pretty = struct
    module Elt = struct
      type 'a t =
        { key : Key.t
        ; value : 'a
        }
      [@@deriving sexp_of]
    end

    type 'a t =
      { min_allowed_key : Key.t
      ; max_allowed_key : Key.t
      ; elts : 'a Elt.t list
      }
    [@@deriving sexp_of]
  end

  let pretty t =
    let pool = t.pool in
    { Pretty.min_allowed_key = min_allowed_key t
    ; max_allowed_key = max_allowed_key t
    ; elts =
        (let r = ref [] in
         internal_iter t ~f:(fun elt ->
           r
           := { Pretty.Elt.key = Internal_elt.key pool elt
              ; value = Internal_elt.value pool elt
              }
              :: !r);
         List.rev !r)
    }
  ;;

  let sexp_of_t sexp_of_a t =
    match !sexp_of_t_style with
    | `Internal -> [%sexp (t : a t_internal)]
    | `Pretty -> [%sexp (pretty t : a Pretty.t)]
  ;;

  let compute_diff_max_min_allowed_key ~level_bits ~bits_per_slot =
    let bits = Num_key_bits.( + ) level_bits bits_per_slot in
    if Num_key_bits.equal bits Num_key_bits.max_value
    then Key.Span.max_value
    else Key.Span.pred (Key.num_keys bits)
  ;;

  let invariant invariant_a t : unit =
    let pool = t.pool in
    let level_invariant level =
      Invariant.invariant [%here] level [%sexp_of: _ Level.t] (fun () ->
        let check f = Invariant.check_field level f in
        Level.Fields.iter
          ~index:(check (fun index -> assert (index >= 0)))
          ~bits:
            (check (fun bits -> assert (Num_key_bits.( > ) bits Num_key_bits.zero)))
          ~slots_mask:
            (check
               ([%test_result: Slots_mask.t]
                  ~expect:(Slots_mask.create ~level_bits:level.bits)))
          ~bits_per_slot:
            (check (fun bits_per_slot ->
               assert (Num_key_bits.( >= ) bits_per_slot Num_key_bits.zero)))
          ~keys_per_slot:
            (check (fun keys_per_slot ->
               [%test_result: Key.Span.t]
                 keys_per_slot
                 ~expect:(Key.num_keys level.bits_per_slot)))
          ~min_key_in_same_slot_mask:
            (check (fun min_key_in_same_slot_mask ->
               assert (
                 Min_key_in_same_slot_mask.equal
                   min_key_in_same_slot_mask
                   (Min_key_in_same_slot_mask.create
                      ~bits_per_slot:level.bits_per_slot))))
          ~diff_max_min_allowed_key:
            (check
               ([%test_result: Key.Span.t]
                  ~expect:
                    (compute_diff_max_min_allowed_key
                       ~level_bits:level.bits
                       ~bits_per_slot:level.bits_per_slot)))
          ~length:
            (check (fun length ->
               assert (
                 length
                 = Array.fold level.slots ~init:0 ~f:(fun n elt ->
                   if Internal_elt.is_null elt
                   then n
                   else n + Internal_elt.length pool elt))))
          ~min_allowed_key:
            (check (fun min_allowed_key ->
               assert (Key.( >= ) min_allowed_key Key.zero);
               if Key.( < ) min_allowed_key Key.max_value
               then
                 [%test_result: Key.Span.t]
                   (Key.rem min_allowed_key level.keys_per_slot)
                   ~expect:Key.Span.zero))
          ~max_allowed_key:
            (check (fun max_allowed_key ->
               [%test_result: Key.t]
                 max_allowed_key
                 ~expect:
                   (Key.add_clamp_to_max
                      level.min_allowed_key
                      level.diff_max_min_allowed_key)))
          ~slots:
            (check (fun slots ->
               Array.iter slots ~f:(fun elt ->
                 if not (Internal_elt.is_null elt)
                 then (
                   Internal_elt.invariant pool invariant_a elt;
                   Internal_elt.iter pool elt ~f:(fun elt ->
                     assert (
                       Key.( >= )
                         (Internal_elt.key pool elt)
                         level.min_allowed_key);
                     assert (
                       Key.( <= )
                         (Internal_elt.key pool elt)
                         level.max_allowed_key);
                     assert (
                       Key.( >= )
                         (Internal_elt.key pool elt)
                         t.elt_key_lower_bound);
                     assert (Internal_elt.level_index pool elt = level.index);
                     invariant_a (Internal_elt.value pool elt)))))))
    in
    Invariant.invariant [%here] t [%sexp_of: _ t_internal] (fun () ->
      let check f = Invariant.check_field t f in
      assert (Key.( >= ) (min_allowed_key t) Key.zero);
      assert (Key.( >= ) (max_allowed_key t) (min_allowed_key t));
      Fields.iter
        ~length:(check (fun length -> assert (length >= 0)))
        ~pool:(check (Internal_elt.Pool.invariant ignore))
        ~min_elt:
          (check (fun elt_ ->
             if not (Internal_elt.is_null elt_)
             then (
               assert (Internal_elt.is_valid t.pool elt_);
               assert (Key.equal t.elt_key_lower_bound (Internal_elt.key t.pool elt_)))))
        ~elt_key_lower_bound:
          (check (fun elt_key_lower_bound ->
             assert (Key.( >= ) elt_key_lower_bound (min_allowed_key t));
             assert (Key.( <= ) elt_key_lower_bound (max_allowed_key t));
             if not (Internal_elt.is_null t.min_elt)
             then
               assert (
                 Key.equal elt_key_lower_bound (Internal_elt.key t.pool t.min_elt))))
        ~levels:
          (check (fun levels ->
             assert (num_levels t > 0);
             Array.iteri levels ~f:(fun level_index level ->
               assert (level_index = Level.index level);
               level_invariant level;
               if level_index > 0
               then (
                 let prev_level = levels.(level_index - 1) in
                 let module L = Level in
                 [%test_result: Key.Span.t]
                   (L.keys_per_slot level)
                   ~expect:(Key.Span.succ prev_level.diff_max_min_allowed_key);
                 [%test_result: Key.t]
                   level.min_allowed_key
                   ~expect:
                     (Level.compute_min_allowed_key
                        level
                        ~prev_level_max_allowed_key:prev_level.max_allowed_key))))))
  ;;

  (** [min_elt_] returns [null] if it can't find the desired element.  We wrap it up
      afterwards to return an [option]. *)
  let min_elt_ t =
    if is_empty t
    then Internal_elt.null ()
    else if not (Internal_elt.is_null t.min_elt)
    then t.min_elt
    else (
      let pool = t.pool in
      let min_elt_already_found = ref (Internal_elt.null ()) in
      let min_key_already_found = ref Key.max_value in
      let level_index = ref 0 in
      let num_levels = num_levels t in
      while !level_index < num_levels do
        let level = t.levels.(!level_index) in
        if Key.( > ) (Level.min_allowed_key level) !min_key_already_found
        then
          (* We don't need to consider any more levels.  Quit the loop. *)
          level_index := num_levels
        else if level.length = 0
        then incr level_index
        else (
          (* Look in [level]. *)
          let slots = level.slots in
          let slot_min_key =
            ref
              (Level.min_key_in_same_slot
                 level
                 ~key:(Key.max level.min_allowed_key t.elt_key_lower_bound))
          in
          let slot = ref (Level.slot level ~key:!slot_min_key) in
          (* Find the first nonempty slot with a small enough [slot_min_key]. *)
          while
            Internal_elt.is_null slots.(!slot)
            && Key.( < ) !slot_min_key !min_key_already_found
          do
            slot := Level.next_slot level !slot;
            slot_min_key := Key.add !slot_min_key level.keys_per_slot
          done;
          let first = slots.(!slot) in
          if not (Internal_elt.is_null first)
          then (
            (* Visit all of the elts in this slot and find one with minimum key. *)
            let continue = ref true in
            let current = ref first in
            while !continue do
              let current_key = Internal_elt.key pool !current in
              if Key.( <= ) current_key !min_key_already_found
              then (
                min_elt_already_found := !current;
                min_key_already_found := current_key);
              let next = Internal_elt.next pool !current in
              (* If [!level_index = 0] then all elts in this slot have the same [key],
                 i.e. [!slot_min_key].  So, we don't have to check any elements after
                 [first].  This is a useful short cut in the common case that there are
                 multiple elements in the same min slot in level 0. *)
              if phys_equal next first || !level_index = 0
              then continue := false
              else current := next
            done);
          (* Finished looking in [level].  Move up to the next level. *)
          incr level_index)
      done;
      t.min_elt <- !min_elt_already_found;
      t.elt_key_lower_bound <- !min_key_already_found;
      t.min_elt)
  ;;

  let[@cold] raise_add_elt_key_out_of_bounds t key =
    raise_s
      [%message
        "Priority_queue.add_elt key out of bounds"
          (key : Key.t)
          (min_allowed_key t : Key.t)
          (max_allowed_key t : Key.t)
          ~priority_queue:(t : _ t)]
  ;;

  let[@cold] raise_add_elt_key_out_of_level_bounds key level =
    raise_s
      [%message
        "Priority_queue.add_elt key out of level bounds"
          (key : Key.t)
          (level : _ Level.t)]
  ;;

  let add_elt t elt =
    let pool = t.pool in
    let key = Internal_elt.key pool elt in
    if not (Key.( >= ) key (min_allowed_key t) && Key.( <= ) key (max_allowed_key t))
    then raise_add_elt_key_out_of_bounds t key;
    (* Find the lowest level that will hold [elt]. *)
    let level_index =
      let level_index = ref 0 in
      while Key.( > ) key (Level.max_allowed_key t.levels.(!level_index)) do
        incr level_index
      done;
      !level_index
    in
    let level = t.levels.(level_index) in
    if not (Key.( >= ) key level.min_allowed_key && Key.( <= ) key level.max_allowed_key)
    then raise_add_elt_key_out_of_level_bounds key level;
    level.length <- level.length + 1;
    Internal_elt.set_level_index pool elt level_index;
    let slot = Level.slot level ~key in
    let slots = level.slots in
    let first = slots.(slot) in
    if not (Internal_elt.is_null first)
    then Internal_elt.insert_at_end pool first ~to_add:elt
    else (
      slots.(slot) <- elt;
      Internal_elt.link_to_self pool elt)
  ;;

  let internal_add_elt t elt =
    let key = Internal_elt.key t.pool elt in
    if Key.( < ) key t.elt_key_lower_bound
    then (
      t.min_elt <- elt;
      t.elt_key_lower_bound <- key);
    add_elt t elt;
    t.length <- t.length + 1
  ;;

  let[@cold] raise_got_invalid_key t key =
    raise_s
      [%message
        "Timing_wheel.add_at_interval_num got invalid interval num"
          ~interval_num:(key : Key.t)
          ~min_allowed_alarm_interval_num:(min_allowed_key t : Key.t)
          ~max_allowed_alarm_interval_num:(max_allowed_key t : Key.t)]
  ;;

  let ensure_valid_key t ~key =
    if Key.( < ) key (min_allowed_key t) || Key.( > ) key (max_allowed_key t)
    then raise_got_invalid_key t key
  ;;

  let internal_add t ~key ~at value =
    ensure_valid_key t ~key;
    if Internal_elt.Pool.is_full t.pool then t.pool <- Internal_elt.Pool.grow t.pool;
    let elt = Internal_elt.create t.pool ~key ~at ~value ~level_index:(-1) in
    internal_add_elt t elt;
    elt
  ;;

  (** [remove_or_re_add_elts] visits each element in the circular doubly-linked list
      [first].  If the element's key is [>= t_min_allowed_key], then it adds the element
      back at a lower level.  If not, then it calls [handle_removed] and [free]s the
      element. *)
  let remove_or_re_add_elts
        t
        (level : _ Level.t)
        first
        ~t_min_allowed_key
        ~handle_removed
    =
    let pool = t.pool in
    let current = ref first in
    let continue = ref true in
    while !continue do
      (* We extract [next] from [current] first, because we will modify or [free]
         [current] before continuing the loop. *)
      let next = Internal_elt.next pool !current in
      level.length <- level.length - 1;
      if Key.( >= ) (Internal_elt.key pool !current) t_min_allowed_key
      then add_elt t !current
      else (
        t.length <- t.length - 1;
        handle_removed (Internal_elt.to_external !current);
        Internal_elt.free pool !current);
      if phys_equal next first then continue := false else current := next
    done
  ;;

  (** [increase_level_min_allowed_key] increases the [min_allowed_key] of [level] to as
      large a value as possible, but no more than [max_level_min_allowed_key].
      [t_min_allowed_key] is the minimum allowed key for the entire timing wheel.  As
      elements are encountered, they are removed from the timing wheel if their key is
      smaller than [t_min_allowed_key], or added at a lower level if not. *)
  let increase_level_min_allowed_key
        t
        (level : _ Level.t)
        ~prev_level_max_allowed_key
        ~t_min_allowed_key
        ~handle_removed
    =
    let desired_min_allowed_key =
      Level.compute_min_allowed_key level ~prev_level_max_allowed_key
    in
    (* We require that [mod level.min_allowed_key level.keys_per_slot = 0].  So,
       we start [level_min_allowed_key] where that is true, and then increase it by
       [keys_per_slot] each iteration of the loop. *)
    let level_min_allowed_key =
      Level.min_key_in_same_slot
        level
        ~key:
          (Key.min
             desired_min_allowed_key
             (Key.max level.min_allowed_key t.elt_key_lower_bound))
    in
    let level_min_allowed_key = ref level_min_allowed_key in
    let slot = ref (Level.slot level ~key:!level_min_allowed_key) in
    let keys_per_slot = level.keys_per_slot in
    let slots = level.slots in
    while Key.( < ) !level_min_allowed_key desired_min_allowed_key do
      if level.length = 0
      then
        (* If no elements remain at this level, we can just set [min_allowed_key] to the
           desired value. *)
        level_min_allowed_key := desired_min_allowed_key
      else (
        let first = slots.(!slot) in
        if not (Internal_elt.is_null first)
        then (
          slots.(!slot) <- Internal_elt.null ();
          remove_or_re_add_elts t level first ~t_min_allowed_key ~handle_removed);
        slot := Level.next_slot level !slot;
        level_min_allowed_key
        := Key.add_clamp_to_max !level_min_allowed_key keys_per_slot)
    done;
    level.min_allowed_key <- desired_min_allowed_key;
    level.max_allowed_key
    <- Key.add_clamp_to_max desired_min_allowed_key level.diff_max_min_allowed_key
  ;;

  let increase_min_allowed_key t ~key ~handle_removed =
    if Key.( > ) key (min_allowed_key t)
    then (
      (* We increase the [min_allowed_key] of levels in order to restore the invariant
         that they have as large as possible a [min_allowed_key], while leaving no gaps
         in keys. *)
      let level_index = ref 0 in
      let prev_level_max_allowed_key = ref (Key.pred key) in
      let levels = t.levels in
      let num_levels = num_levels t in
      while !level_index < num_levels do
        let level = levels.(!level_index) in
        let min_allowed_key_before = level.min_allowed_key in
        increase_level_min_allowed_key
          t
          level
          ~prev_level_max_allowed_key:!prev_level_max_allowed_key
          ~t_min_allowed_key:key
          ~handle_removed;
        if Key.equal (Level.min_allowed_key level) min_allowed_key_before
        then
          (* This level did not shift.  Don't shift any higher levels. *)
          level_index := num_levels
        else (
          (* Level [level_index] shifted.  Consider shifting higher levels. *)
          level_index := !level_index + 1;
          prev_level_max_allowed_key := Level.max_allowed_key level)
      done;
      if Key.( > ) key t.elt_key_lower_bound
      then (
        (* We have removed [t.min_elt] or it was already null, so just set it to
           null. *)
        t.min_elt <- Internal_elt.null ();
        t.elt_key_lower_bound <- min_allowed_key t))
  ;;

  let create ?capacity ?level_bits () =
    let level_bits =
      match level_bits with
      | Some l -> l
      | None -> Level_bits.default
    in
    let _, _, levels =
      List.foldi
        level_bits
        ~init:(Num_key_bits.zero, Key.zero, [])
        ~f:(fun index
             (bits_per_slot, max_level_min_allowed_key, levels)
             (level_bits : Num_key_bits.t)
             ->
               let keys_per_slot = Key.num_keys bits_per_slot in
               let diff_max_min_allowed_key =
                 compute_diff_max_min_allowed_key ~level_bits ~bits_per_slot
               in
               let min_key_in_same_slot_mask =
                 Min_key_in_same_slot_mask.create ~bits_per_slot
               in
               let min_allowed_key =
                 Key.min_key_in_same_slot max_level_min_allowed_key min_key_in_same_slot_mask
               in
               let max_allowed_key =
                 Key.add_clamp_to_max min_allowed_key diff_max_min_allowed_key
               in
               let level =
                 { Level.index
                 ; bits = level_bits
                 ; slots_mask = Slots_mask.create ~level_bits
                 ; bits_per_slot
                 ; keys_per_slot
                 ; min_key_in_same_slot_mask
                 ; diff_max_min_allowed_key
                 ; length = 0
                 ; min_allowed_key
                 ; max_allowed_key
                 ; slots =
                     Array.create
                       ~len:(Int63.to_int_exn (Num_key_bits.pow2 level_bits))
                       (Internal_elt.null ())
                 }
               in
               ( Num_key_bits.( + ) level_bits bits_per_slot
               , Key.succ_clamp_to_max max_allowed_key
               , level :: levels ))
    in
    { length = 0
    ; pool = Internal_elt.Pool.create ?capacity ()
    ; min_elt = Internal_elt.null ()
    ; elt_key_lower_bound = Key.zero
    ; levels = Array.of_list_rev levels
    }
  ;;

  let mem t elt = Internal_elt.external_is_valid t.pool elt

  let internal_remove t elt =
    let pool = t.pool in
    if Internal_elt.equal elt t.min_elt
    then
      t.min_elt <- Internal_elt.null ()
    (* We keep [t.elt_lower_bound] since it is valid even though [t.min_elt] is being
       removed. *);
    t.length <- t.length - 1;
    let level = t.levels.(Internal_elt.level_index pool elt) in
    level.length <- level.length - 1;
    let slots = level.slots in
    let slot = Level.slot level ~key:(Internal_elt.key pool elt) in
    let first = slots.(slot) in
    if phys_equal elt (Internal_elt.next pool elt)
    then (* [elt] is the only element in the slot *)
      slots.(slot) <- Internal_elt.null ()
    else (
      if phys_equal elt first then slots.(slot) <- Internal_elt.next pool elt;
      Internal_elt.unlink pool elt)
  ;;

  let remove t elt =
    let pool = t.pool in
    let elt = Internal_elt.of_external_exn pool elt in
    internal_remove t elt;
    Internal_elt.free pool elt
  ;;

  let fire_past_alarms t ~handle_fired ~key ~now =
    let level = t.levels.(0) in
    if level.length > 0
    then (
      let slot = Level.slot level ~key in
      let slots = level.slots in
      let pool = t.pool in
      let first = ref slots.(slot) in
      if not (Internal_elt.is_null !first)
      then (
        let current = ref !first in
        let continue = ref true in
        while !continue do
          let elt = !current in
          let next = Internal_elt.next pool elt in
          if phys_equal next !first then continue := false else current := next;
          if Time_ns.( <= ) (Internal_elt.at pool elt) now
          then (
            handle_fired (Internal_elt.to_external elt);
            internal_remove t elt;
            Internal_elt.free pool elt;
            (* We recompute [first] because [internal_remove] may have changed it. *)
            first := slots.(slot))
        done))
  ;;

  let change t elt ~key ~at =
    ensure_valid_key t ~key;
    let pool = t.pool in
    let elt = Internal_elt.of_external_exn pool elt in
    internal_remove t elt;
    Internal_elt.set_key pool elt key;
    Internal_elt.set_at pool elt at;
    internal_add_elt t elt
  ;;

  let clear t =
    if not (is_empty t)
    then (
      t.length <- 0;
      let pool = t.pool in
      let free_elt elt = Internal_elt.free pool elt in
      let levels = t.levels in
      for level_index = 0 to Array.length levels - 1 do
        let level = levels.(level_index) in
        if level.length > 0
        then (
          level.length <- 0;
          let slots = level.slots in
          for slot_index = 0 to Array.length slots - 1 do
            let elt = slots.(slot_index) in
            if not (Internal_elt.is_null elt)
            then (
              Internal_elt.iter pool elt ~f:free_elt;
              slots.(slot_index) <- Internal_elt.null ())
          done)
      done)
  ;;
end

module Internal_elt = Priority_queue.Internal_elt
module Key = Priority_queue.Key
module Interval_num = Key

let min_interval_num = Interval_num.zero

(* All time from the epoch onwards is broken into half-open intervals of size
   [Config.alarm_precision config].  The intervals are numbered starting at zero, and a
   time's interval number serves as its key in [priority_queue]. *)
type 'a t =
  { config : Config.t
  ; start : Time_ns.t
  ; (* [max_interval_num] is the interval number of [max_time]. *)
    max_interval_num : Interval_num.t
  ; mutable now : Time_ns.t
  ; mutable now_interval_num_start : Time_ns.t
  ; mutable max_allowed_alarm_time : Time_ns.t
  ; priority_queue : 'a Priority_queue.t
  }
[@@deriving fields, sexp_of]

type 'a timing_wheel = 'a t
type 'a t_now = 'a t

let sexp_of_t_now _ t = [%sexp (t.now : Time_ns.t)]
let alarm_precision t = Config.alarm_precision t.config

module Alarm = struct
  type 'a t = 'a Priority_queue.Elt.t [@@deriving sexp_of]

  let null = Priority_queue.Elt.null
  let at tw t = Priority_queue.Elt.at tw.priority_queue t
  let value tw t = Priority_queue.Elt.value tw.priority_queue t
  let interval_num tw t = Priority_queue.Elt.key tw.priority_queue t
end

let sexp_of_t_internal = sexp_of_t
let iter t ~f = Priority_queue.iter t.priority_queue ~f

module Pretty = struct
  module Alarm = struct
    type 'a t =
      { at : Time_ns.t
      ; value : 'a
      }
    [@@deriving fields, sexp_of]

    let create t alarm = { at = Alarm.at t alarm; value = Alarm.value t alarm }
    let compare t1 t2 = Time_ns.compare (at t1) (at t2)
  end

  type 'a t =
    { config : Config.t
    ; start : Time_ns.t
    ; max_interval_num : Interval_num.t
    ; now : Time_ns.t
    ; alarms : 'a Alarm.t list
    }
  [@@deriving sexp_of]
end

let pretty
      ({ config
       ; start
       ; max_interval_num
       ; now
       ; now_interval_num_start = _
       ; max_allowed_alarm_time = _
       ; priority_queue = _
       } as t)
  =
  let r = ref [] in
  iter t ~f:(fun a -> r := Pretty.Alarm.create t a :: !r);
  let alarms = List.sort !r ~compare:Pretty.Alarm.compare in
  { Pretty.config; start; max_interval_num; now; alarms }
;;

let sexp_of_t sexp_of_a t =
  match !sexp_of_t_style with
  | `Internal -> sexp_of_t_internal sexp_of_a t
  | `Pretty -> [%sexp (pretty t : a Pretty.t)]
;;

let length t = Priority_queue.length t.priority_queue
let is_empty t = length t = 0

let interval_num_internal ~time ~alarm_precision =
  Interval_num.of_int63 (Alarm_precision.interval_num alarm_precision time)
;;

let interval_num_unchecked t time =
  interval_num_internal ~time ~alarm_precision:t.config.alarm_precision
;;

let interval_num t time =
  if Time_ns.( < ) time min_time
  then
    raise_s
      [%message
        "Timing_wheel.interval_num got time too far in the past" (time : Time_ns.t)];
  interval_num_unchecked t time
;;

let interval_num_start_unchecked t interval_num =
  Alarm_precision.interval_num_start
    t.config.alarm_precision
    (interval_num |> Interval_num.to_int63)
;;

let[@cold] raise_interval_num_start_got_too_small interval_num =
  raise_s
    [%message
      "Timing_wheel.interval_num_start got too small interval_num"
        (interval_num : Interval_num.t)
        (min_interval_num : Interval_num.t)]
;;

let[@cold] raise_interval_num_start_got_too_large t interval_num =
  raise_s
    [%message
      "Timing_wheel.interval_num_start got too large interval_num"
        (interval_num : Interval_num.t)
        (t.max_interval_num : Interval_num.t)]
;;

let interval_num_start t interval_num =
  if Interval_num.( < ) interval_num min_interval_num
  then raise_interval_num_start_got_too_small interval_num;
  if Interval_num.( > ) interval_num t.max_interval_num
  then raise_interval_num_start_got_too_large t interval_num;
  interval_num_start_unchecked t interval_num
;;

let compute_max_allowed_alarm_time t =
  let max_allowed_key = Priority_queue.max_allowed_key t.priority_queue in
  if Interval_num.( >= ) max_allowed_key t.max_interval_num
  then max_time
  else
    Time_ns.add
      (interval_num_start_unchecked t max_allowed_key)
      (Time_ns.Span.( - ) (alarm_precision t) Time_ns.Span.nanosecond)
;;

let now_interval_num t = Priority_queue.min_allowed_key t.priority_queue
let min_allowed_alarm_interval_num = now_interval_num
let max_allowed_alarm_interval_num t = interval_num t (max_allowed_alarm_time t)
let interval_start t time = interval_num_start_unchecked t (interval_num t time)

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~config:(check Config.invariant)
      ~start:
        (check (fun start ->
           assert (Time_ns.( >= ) start min_time);
           assert (Time_ns.( <= ) start max_time)))
      ~max_interval_num:
        (check (fun max_interval_num ->
           [%test_result: Interval_num.t]
             ~expect:max_interval_num
             (interval_num t max_time);
           [%test_result: Interval_num.t]
             ~expect:max_interval_num
             (interval_num t (interval_num_start t max_interval_num))))
      ~now:
        (check (fun now ->
           assert (Time_ns.( >= ) now t.start);
           assert (Time_ns.( <= ) now max_time);
           assert (
             Interval_num.equal
               (interval_num t t.now)
               (Priority_queue.min_allowed_key t.priority_queue))))
      ~now_interval_num_start:
        (check (fun now_interval_num_start ->
           [%test_result: Time_ns.t]
             now_interval_num_start
             ~expect:(interval_num_start t (now_interval_num t))))
      ~max_allowed_alarm_time:
        (check (fun max_allowed_alarm_time ->
           [%test_result: Time_ns.t]
             max_allowed_alarm_time
             ~expect:(compute_max_allowed_alarm_time t)))
      ~priority_queue:(check (Priority_queue.invariant invariant_a));
    iter t ~f:(fun alarm ->
      assert (
        Interval_num.equal
          (Alarm.interval_num t alarm)
          (interval_num t (Alarm.at t alarm)));
      assert (
        Time_ns.( >= )
          (interval_start t (Alarm.at t alarm))
          (interval_start t (now t)));
      assert (
        Time_ns.( > ) (Alarm.at t alarm) (Time_ns.sub (now t) (alarm_precision t)))))
;;

let advance_clock t ~to_ ~handle_fired =
  if Time_ns.( > ) to_ (now t)
  then (
    t.now <- to_;
    let key = interval_num_unchecked t to_ in
    t.now_interval_num_start <- interval_num_start_unchecked t key;
    Priority_queue.increase_min_allowed_key
      t.priority_queue
      ~key
      ~handle_removed:handle_fired;
    t.max_allowed_alarm_time <- compute_max_allowed_alarm_time t)
;;

let create ~config ~start =
  if Time_ns.( < ) start Time_ns.epoch
  then
    raise_s
      [%message "Timing_wheel.create got start before the epoch" (start : Time_ns.t)];
  let t =
    { config
    ; start
    ; max_interval_num =
        interval_num_internal ~time:max_time ~alarm_precision:config.alarm_precision
    ; now = Time_ns.min_value_for_1us_rounding (* set by [advance_clock] below *)
    ; now_interval_num_start =
        Time_ns.min_value_for_1us_rounding (* set by [advance_clock] below *)
    ; max_allowed_alarm_time = max_time (* set by [advance_clock] below *)
    ; priority_queue =
        Priority_queue.create ?capacity:config.capacity ~level_bits:config.level_bits ()
    }
  in
  advance_clock t ~to_:start ~handle_fired:(fun _ -> assert false);
  t
;;

let add_at_interval_num t ~at value =
  Internal_elt.to_external
    (Priority_queue.internal_add
       t.priority_queue
       ~key:at
       ~at:(interval_num_start t at)
       value)
;;

let[@cold] raise_that_far_in_the_future t at =
  raise_s
    [%message
      "Timing_wheel cannot schedule alarm that far in the future"
        (at : Time_ns.t)
        ~max_allowed_alarm_time:(t.max_allowed_alarm_time : Time_ns.t)]
;;

let[@cold] raise_before_start_of_current_interval t at =
  raise_s
    [%message
      "Timing_wheel cannot schedule alarm before start of current interval"
        (at : Time_ns.t)
        ~now_interval_num_start:(t.now_interval_num_start : Time_ns.t)]
;;

let ensure_can_schedule_alarm t ~at =
  if Time_ns.( > ) at t.max_allowed_alarm_time then raise_that_far_in_the_future t at;
  if Time_ns.( < ) at t.now_interval_num_start
  then raise_before_start_of_current_interval t at
;;

let add t ~at value =
  ensure_can_schedule_alarm t ~at;
  Internal_elt.to_external
    (Priority_queue.internal_add
       t.priority_queue
       ~key:(interval_num_unchecked t at)
       ~at
       value)
;;

let remove t alarm = Priority_queue.remove t.priority_queue alarm
let clear t = Priority_queue.clear t.priority_queue
let mem t alarm = Priority_queue.mem t.priority_queue alarm

let reschedule_gen t alarm ~key ~at =
  if not (mem t alarm)
  then failwith "Timing_wheel cannot reschedule alarm not in timing wheel";
  ensure_can_schedule_alarm t ~at;
  Priority_queue.change t.priority_queue alarm ~key ~at
;;

let reschedule t alarm ~at =
  reschedule_gen t alarm ~key:(interval_num_unchecked t at) ~at
;;

let reschedule_at_interval_num t alarm ~at =
  reschedule_gen t alarm ~key:at ~at:(interval_num_start t at)
;;

let pool t = Priority_queue.pool t.priority_queue

let min_alarm_interval_num t =
  let elt = Priority_queue.min_elt_ t.priority_queue in
  if Internal_elt.is_null elt then None else Some (Internal_elt.key (pool t) elt)
;;

let min_alarm_interval_num_exn t =
  let elt = Priority_queue.min_elt_ t.priority_queue in
  if Internal_elt.is_null elt
  then
    raise_s
      [%message
        "Timing_wheel.min_alarm_interval_num_exn of empty timing_wheel"
          ~timing_wheel:(t : _ t)]
  else Internal_elt.key (pool t) elt
;;

let max_alarm_time_in_list t elt =
  let pool = pool t in
  Internal_elt.max_alarm_time pool elt ~with_key:(Internal_elt.key pool elt)
;;

let max_alarm_time_in_min_interval t =
  let elt = Priority_queue.min_elt_ t.priority_queue in
  if Internal_elt.is_null elt then None else Some (max_alarm_time_in_list t elt)
;;

let max_alarm_time_in_min_interval_exn t =
  let elt = Priority_queue.min_elt_ t.priority_queue in
  if Internal_elt.is_null elt
  then
    raise_s
      [%message
        "Timing_wheel.max_alarm_time_in_min_interval_exn of empty timing wheel"
          ~timing_wheel:(t : _ t)];
  max_alarm_time_in_list t elt
;;

let next_alarm_fires_at_internal t key =
  (* [interval_num_start t key] is the key corresponding to the start of the time interval
     holding the first alarm in [t].  Advancing to that would not be enough, since the
     alarms in that interval don't fire until the clock is advanced to the start of the
     next interval.  So, we use [succ key] to advance to the start of the next
     interval. *)
  interval_num_start t (Key.succ key)
;;

let next_alarm_fires_at t =
  let elt = Priority_queue.min_elt_ t.priority_queue in
  if Internal_elt.is_null elt
  then None
  else (
    let key = Internal_elt.key (pool t) elt in
    if Interval_num.equal key t.max_interval_num
    then None
    else Some (next_alarm_fires_at_internal t key))
;;

let[@cold] raise_next_alarm_fires_at_exn_of_empty_timing_wheel t =
  raise_s
    [%message
      "Timing_wheel.next_alarm_fires_at_exn of empty timing wheel"
        ~timing_wheel:(t : _ t)]
;;

let[@cold] raise_next_alarm_fires_at_with_all_alarms_in_max_interval t =
  raise_s
    [%message
      "Timing_wheel.next_alarm_fires_at_exn with all alarms in max interval"
        ~timing_wheel:(t : _ t)]
;;

let next_alarm_fires_at_exn t =
  let elt = Priority_queue.min_elt_ t.priority_queue in
  if Internal_elt.is_null elt then raise_next_alarm_fires_at_exn_of_empty_timing_wheel t;
  let key = Internal_elt.key (pool t) elt in
  if Interval_num.equal key t.max_interval_num
  then raise_next_alarm_fires_at_with_all_alarms_in_max_interval t;
  next_alarm_fires_at_internal t key
;;

let fire_past_alarms t ~handle_fired =
  Priority_queue.fire_past_alarms
    t.priority_queue
    ~handle_fired
    ~key:(now_interval_num t)
    ~now:t.now
;;

module Private = struct
  module Num_key_bits = Num_key_bits

  let interval_num_internal = interval_num_internal
  let max_time = max_time
end

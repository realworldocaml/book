(** Mutable vector of elements with O(1) [get] and [set] operations. *)

open! Import

type 'a t = 'a array [@@deriving_inline compare, sexp, sexp_grammar]

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t := 'a t

val t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t

[@@@end]

include Binary_searchable.S1 with type 'a t := 'a t


include Container.S1 with type 'a t := 'a t
include Invariant.S1 with type 'a t := 'a t

(** Maximum length of a normal array.  The maximum length of a float array is
    [max_length/2] on 32-bit machines and [max_length] on 64-bit machines. *)
val max_length : int

(** [Array.get a n] returns the element number [n] of array [a].
    The first element has number 0.
    The last element has number [Array.length a - 1].
    You can also write [a.(n)] instead of [Array.get a n].

    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [(Array.length a - 1)]. *)
external get : 'a t -> int -> 'a = "%array_safe_get"

(** [Array.set a n x] modifies array [a] in place, replacing
    element number [n] with [x].
    You can also write [a.(n) <- x] instead of [Array.set a n x].

    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [Array.length a - 1]. *)
external set : 'a t -> int -> 'a -> unit = "%array_safe_set"

(** Unsafe version of [get].  Can cause arbitrary behavior when used for an out-of-bounds
    array access. *)
external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"

(** Unsafe version of [set].  Can cause arbitrary behavior when used for an out-of-bounds
    array access. *)
external unsafe_set : 'a t -> int -> 'a -> unit = "%array_unsafe_set"

(** [create ~len x] creates an array of length [len] with the value [x] populated in
    each element. *)
val create : len:int -> 'a -> 'a t

(** [init n ~f] creates an array of length [n] where the [i]th element (starting at zero)
    is initialized with [f i]. *)
val init : int -> f:(int -> 'a) -> 'a t

(** [Array.make_matrix dimx dimy e] returns a two-dimensional array (an array of arrays)
    with first dimension [dimx] and second dimension [dimy]. All the elements of this new
    matrix are initially physically equal to [e].  The element ([x,y]) of a matrix [m] is
    accessed with the notation [m.(x).(y)].

    Raise [Invalid_argument] if [dimx] or [dimy] is negative or greater than
    [Array.max_length].

    If the value of [e] is a floating-point number, then the maximum size is only
    [Array.max_length / 2]. *)
val make_matrix : dimx:int -> dimy:int -> 'a -> 'a t t

(** [Array.append v1 v2] returns a fresh array containing the concatenation of the arrays
    [v1] and [v2]. *)
val append : 'a t -> 'a t -> 'a t

(** Like [Array.append], but concatenates a list of arrays. *)
val concat : 'a t list -> 'a t

(** [Array.copy a] returns a copy of [a], that is, a fresh array
    containing the same elements as [a]. *)
val copy : 'a t -> 'a t

(** [Array.fill a ofs len x] modifies the array [a] in place, storing [x] in elements
    number [ofs] to [ofs + len - 1].

    Raise [Invalid_argument "Array.fill"] if [ofs] and [len] do not designate a valid
    subarray of [a]. *)
val fill : 'a t -> pos:int -> len:int -> 'a -> unit

(** [Array.blit v1 o1 v2 o2 len] copies [len] elements from array [v1], starting at
    element number [o1], to array [v2], starting at element number [o2].  It works
    correctly even if [v1] and [v2] are the same array, and the source and destination
    chunks overlap.

    Raise [Invalid_argument "Array.blit"] if [o1] and [len] do not designate a valid
    subarray of [v1], or if [o2] and [len] do not designate a valid subarray of [v2].

    [int_blit] and [float_blit] provide fast bound-checked blits for immediate
    data types.  The unsafe versions do not bound-check the arguments. *)
include
  Blit.S1 with type 'a t := 'a t

(** [Array.of_list l] returns a fresh array containing the elements of [l]. *)
val of_list : 'a list -> 'a t

(** [Array.map t ~f] applies function [f] to all the elements of [t], and builds an array
    with the results returned by [f]: [[| f t.(0); f t.(1); ...; f t.(Array.length t - 1)
    |]]. *)
val map : 'a t -> f:('a -> 'b) -> 'b t

(** [folding_map] is a version of [map] that threads an accumulator through calls to
    [f]. *)
val folding_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'c t

val folding_mapi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b * 'c) -> 'c t

(** [Array.fold_map] is a combination of [Array.fold] and [Array.map] that threads an
    accumulator through calls to [f]. *)
val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'b * 'c t

val fold_mapi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b * 'c) -> 'b * 'c t

(** Like {!Array.iter}, but the function is applied to the index of the element as first
    argument, and the element itself as second argument. *)
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit

(** Like {!Array.map}, but the function is applied to the index of the element as first
    argument, and the element itself as second argument. *)
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t

val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b

(** [Array.fold_right f a ~init] computes [f a.(0) (f a.(1) ( ... (f a.(n-1) init) ...))],
    where [n] is the length of the array [a]. *)
val fold_right : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b


(** All sort functions in this module sort in increasing order by default.  *)

(** [sort] uses constant heap space. [stable_sort] uses linear heap space.

    To sort only part of the array, specify [pos] to be the index to start sorting from
    and [len] indicating how many elements to sort. *)
val sort : ?pos:int -> ?len:int -> 'a t -> compare:('a -> 'a -> int) -> unit

val stable_sort : 'a t -> compare:('a -> 'a -> int) -> unit
val is_sorted : 'a t -> compare:('a -> 'a -> int) -> bool

(** [is_sorted_strictly xs ~compare] iff [is_sorted xs ~compare] and no two
    consecutive elements in [xs] are equal according to [compare]. *)
val is_sorted_strictly : 'a t -> compare:('a -> 'a -> int) -> bool

(** Like [List.concat_map], [List.concat_mapi]. *)
val concat_map : 'a t -> f:('a -> 'b array) -> 'b array

val concat_mapi : 'a t -> f:(int -> 'a -> 'b array) -> 'b array
val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t
val partitioni_tf : 'a t -> f:(int -> 'a -> bool) -> 'a t * 'a t
val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

(** [transpose] in the sense of a matrix transpose.  It returns [None] if the arrays are
    not all the same length. *)
val transpose : 'a t t -> 'a t t option

val transpose_exn : 'a t t -> 'a t t

(** [filter_opt array] returns a new array where [None] entries are omitted and [Some x]
    entries are replaced with [x]. Note that this changes the index at which elements
    will appear. *)
val filter_opt : 'a option t -> 'a t

(** [filter_map ~f array] maps [f] over [array] and filters [None] out of the
    results. *)
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

(** Like [filter_map] but uses {!Array.mapi}. *)
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t

(** Like [for_all], but passes the index as an argument. *)
val for_alli : 'a t -> f:(int -> 'a -> bool) -> bool

(** Like [exists], but passes the index as an argument. *)
val existsi : 'a t -> f:(int -> 'a -> bool) -> bool

(** Like [count], but passes the index as an argument. *)
val counti : 'a t -> f:(int -> 'a -> bool) -> int

(** Functions with the 2 suffix raise an exception if the lengths of the two given arrays
    aren't the same. *)

val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val fold2_exn : 'a t -> 'b t -> init:'c -> f:('c -> 'a -> 'b -> 'c) -> 'c

(** [for_all2_exn t1 t2 ~f] fails if [length t1 <> length t2]. *)
val for_all2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

(** [exists2_exn t1 t2 ~f] fails if [length t1 <> length t2]. *)
val exists2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

(** [filter t ~f] removes the elements for which [f] returns false.  *)
val filter : 'a t -> f:('a -> bool) -> 'a t

(** Like [filter] except [f] also receives the index. *)
val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t

(** [swap arr i j] swaps the value at index [i] with that at index [j]. *)
val swap : 'a t -> int -> int -> unit

(** [rev_inplace t] reverses [t] in place. *)
val rev_inplace : 'a t -> unit

(** [of_list_rev l] converts from list then reverses in place. *)
val of_list_rev : 'a list -> 'a t

(** [of_list_map l ~f] is the same as [of_list (List.map l ~f)]. *)
val of_list_map : 'a list -> f:('a -> 'b) -> 'b t

(** [of_list_mapi l ~f] is the same as [of_list (List.mapi l ~f)]. *)
val of_list_mapi : 'a list -> f:(int -> 'a -> 'b) -> 'b t

(** [of_list_rev_map l ~f] is the same as [of_list (List.rev_map l ~f)]. *)
val of_list_rev_map : 'a list -> f:('a -> 'b) -> 'b t

(** [of_list_rev_mapi l ~f] is the same as [of_list (List.rev_mapi l ~f)]. *)
val of_list_rev_mapi : 'a list -> f:(int -> 'a -> 'b) -> 'b t

(** Modifies an array in place, applying [f] to every element of the array *)
val map_inplace : 'a t -> f:('a -> 'a) -> unit


(** [find_exn f t] returns the first [a] in [t] for which [f t.(i)] is true.  It raises
    [Caml.Not_found] or [Not_found_s] if there is no such [a]. *)
val find_exn : 'a t -> f:('a -> bool) -> 'a

(** Returns the first evaluation of [f] that returns [Some].  Raises [Caml.Not_found] or
    [Not_found_s] if [f] always returns [None].  *)
val find_map_exn : 'a t -> f:('a -> 'b option) -> 'b

(** [findi t f] returns the first index [i] of [t] for which [f i t.(i)] is true *)
val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option

(** [findi_exn t f] returns the first index [i] of [t] for which [f i t.(i)] is true.  It
    raises [Caml.Not_found] or [Not_found_s] if there is no such element. *)
val findi_exn : 'a t -> f:(int -> 'a -> bool) -> int * 'a

(** [find_mapi t f] is like [find_map] but passes the index as an argument. *)
val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option

(** [find_mapi_exn] is like [find_map_exn] but passes the index as an argument. *)
val find_mapi_exn : 'a t -> f:(int -> 'a -> 'b option) -> 'b

(** [find_consecutive_duplicate t ~equal] returns the first pair of consecutive elements
    [(a1, a2)] in [t] such that [equal a1 a2].  They are returned in the same order as
    they appear in [t]. *)
val find_consecutive_duplicate : 'a t -> equal:('a -> 'a -> bool) -> ('a * 'a) option

(** [reduce f [a1; ...; an]] is [Some (f (... (f (f a1 a2) a3) ...) an)].  Returns [None]
    on the empty array. *)
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option

val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a

(** [permute ?random_state t] randomly permutes [t] in place.

    [permute] side-effects [random_state] by repeated calls to [Random.State.int].  If
    [random_state] is not supplied, [permute] uses [Random.State.default]. *)
val permute : ?random_state:Random.State.t -> 'a t -> unit

(** [random_element ?random_state t] is [None] if [t] is empty, else it is [Some x] for
    some [x] chosen uniformly at random from [t].

    [random_element] side-effects [random_state] by calling [Random.State.int]. If
    [random_state] is not supplied, [random_element] uses [Random.State.default]. *)
val random_element : ?random_state:Random.State.t -> 'a t -> 'a option

val random_element_exn : ?random_state:Random.State.t -> 'a t -> 'a

(** [zip] is like [List.zip], but for arrays. *)
val zip : 'a t -> 'b t -> ('a * 'b) t option

val zip_exn : 'a t -> 'b t -> ('a * 'b) t

(** [unzip] is like [List.unzip], but for arrays. *)
val unzip : ('a * 'b) t -> 'a t * 'b t

(** [sorted_copy ar compare] returns a shallow copy of [ar] that is sorted. Similar to
    List.sort *)
val sorted_copy : 'a t -> compare:('a -> 'a -> int) -> 'a t

val last : 'a t -> 'a
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool


(** The input array is copied internally so that future modifications of it do not change
    the sequence. *)
val to_sequence : 'a t -> 'a Sequence.t

(** The input array is shared with the sequence and modifications of it will result in
    modification of the sequence. *)
val to_sequence_mutable : 'a t -> 'a Sequence.t

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  module Sort : sig
    module type Sort = sig
      val sort : 'a t -> compare:('a -> 'a -> int) -> left:int -> right:int -> unit
    end

    module Insertion_sort : Sort
    module Heap_sort : Sort

    module Intro_sort : sig
      include Sort

      val five_element_sort
        :  'a t
        -> compare:('a -> 'a -> int)
        -> int
        -> int
        -> int
        -> int
        -> int
        -> unit
    end
  end
end

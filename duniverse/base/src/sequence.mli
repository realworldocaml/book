(** A sequence of elements that can be produced one at a time, on demand, normally with no
    sharing.

    The elements are computed on demand, possibly repeating work if they are demanded
    multiple times.  A sequence can be built by unfolding from some initial state, which
    will in practice often be other containers.

    Most functions constructing a sequence will not immediately compute any elements of
    the sequence.  These functions will always return in O(1), but traversing the
    resulting sequence may be more expensive.  The most they will do immediately is
    generate a new internal state and a new step function.

    Functions that transform existing sequences sometimes have to reconstruct some suffix
    of the input sequence, even if it is unmodified.  For example, calling [drop 1] will
    return a sequence with a slightly larger state and whose elements all cost slightly
    more to traverse.  Because this is sometimes undesirable (for example, applying [drop
    1] n times will cost O(n) per element traversed in the result), there are also more
    eager versions of many functions (whose names are suffixed with [_eagerly]) that do
    more work up front.  A function has the [_eagerly] suffix iff it matches both of these
    conditions:

    - It might consume an element from an input [t] before returning.

    - It only returns a [t] (not paired with something else, not wrapped in an [option],
      etc.).  If it returns anything other than a [t] and it has at least one [t] input,
      it's probably demanding elements from the input [t] anyway.

    Only [*_exn] functions can raise exceptions, except if the function underlying the
    sequence (the [f] passed to [unfold]) raises, in which case the exception will
    cascade. *)

open! Import

type +'a t [@@deriving_inline compare, equal, sexp_of]

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val sexp_of_t : ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t

[@@@end]

type 'a sequence = 'a t

include Indexed_container.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

(** [empty] is a sequence with no elements. *)
val empty : _ t

(** [next] returns the next element of a sequence and the next tail if the sequence is not
    finished. *)
val next : 'a t -> ('a * 'a t) option

(** A [Step] describes the next step of the sequence construction.  [Done] indicates the
    sequence is finished.  [Skip] indicates the sequence continues with another state
    without producing the next element yet.  [Yield] outputs an element and introduces a
    new state.

    Modifying ['s] doesn't violate any {e internal} invariants, but it may violate some
    undocumented expectations.  For example, one might expect that producing an element
    from the same point in the sequence would always give the same value, but if the state
    can mutate, that is not so. *)
module Step : sig
  type ('a, 's) t =
    | Done
    | Skip of 's
    | Yield of 'a * 's
  [@@deriving_inline sexp_of]

  val sexp_of_t
    :  ('a -> Ppx_sexp_conv_lib.Sexp.t)
    -> ('s -> Ppx_sexp_conv_lib.Sexp.t)
    -> ('a, 's) t
    -> Ppx_sexp_conv_lib.Sexp.t

  [@@@end]
end

(** [unfold_step ~init ~f] constructs a sequence by giving an initial state [init] and a
    function [f] explaining how to continue the next step from a given state. *)
val unfold_step : init:'s -> f:('s -> ('a, 's) Step.t) -> 'a t

(** [unfold ~init f] is a simplified version of [unfold_step] that does not allow
    [Skip]. *)
val unfold : init:'s -> f:('s -> ('a * 's) option) -> 'a t

(** [unfold_with t ~init ~f] folds a state through the sequence [t] to create a new
    sequence *)
val unfold_with : 'a t -> init:'s -> f:('s -> 'a -> ('b, 's) Step.t) -> 'b t

(** [unfold_with_and_finish t ~init ~running_step ~inner_finished ~finishing_step] folds a
    state through [t] to create a new sequence (like [unfold_with t ~init
    ~f:running_step]), and then continues the new sequence by unfolding the final state
    (like [unfold_step ~init:(inner_finished final_state) ~f:finishing_step]). *)
val unfold_with_and_finish
  :  'a t
  -> init:'s_a
  -> running_step:('s_a -> 'a -> ('b, 's_a) Step.t)
  -> inner_finished:('s_a -> 's_b)
  -> finishing_step:('s_b -> ('b, 's_b) Step.t)
  -> 'b t

(** Returns the nth element. *)
val nth : 'a t -> int -> 'a option

val nth_exn : 'a t -> int -> 'a

(** [folding_map] is a version of [map] that threads an accumulator through calls to
    [f]. *)
val folding_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'c t

val folding_mapi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b * 'c) -> 'c t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t
val filter : 'a t -> f:('a -> bool) -> 'a t

(** [merge t1 t2 ~compare] merges two sorted sequences [t1] and [t2], returning a sorted
    sequence, all according to [compare].  If two elements are equal, the one from [t1] is
    preferred.  The behavior is undefined if the inputs aren't sorted. *)
val merge : 'a t -> 'a t -> compare:('a -> 'a -> int) -> 'a t

module Merge_with_duplicates_element : sig
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
    | Both of 'a * 'b
  [@@deriving_inline compare, hash, sexp]

  val compare : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int

  val hash_fold_t
    :  (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
    -> (Ppx_hash_lib.Std.Hash.state -> 'b -> Ppx_hash_lib.Std.Hash.state)
    -> Ppx_hash_lib.Std.Hash.state
    -> ('a, 'b) t
    -> Ppx_hash_lib.Std.Hash.state

  include Ppx_sexp_conv_lib.Sexpable.S2 with type ('a, 'b) t := ('a, 'b) t

  [@@@end]
end

(** [merge_with_duplicates_element t1 t2 ~compare] is like [merge], except that for each
    element it indicates which input(s) the element comes from, using
    [Merge_with_duplicates_element]. *)
val merge_with_duplicates
  :  'a t
  -> 'b t
  -> compare:('a -> 'b -> int)
  -> ('a, 'b) Merge_with_duplicates_element.t t

val hd : 'a t -> 'a option
val hd_exn : 'a t -> 'a

(** [tl t] and [tl_eagerly_exn t] immediately evaluates the first element of [t] and
    returns the unevaluated tail. *)
val tl : 'a t -> 'a t option

val tl_eagerly_exn : 'a t -> 'a t

(** [find_exn t ~f] returns the first element of [t] that satisfies [f]. It raises if
    there is no such element. *)
val find_exn : 'a t -> f:('a -> bool) -> 'a

(** Like [for_all], but passes the index as an argument. *)
val for_alli : 'a t -> f:(int -> 'a -> bool) -> bool

(** [append t1 t2] first produces the elements of [t1], then produces the elements of
    [t2]. *)
val append : 'a t -> 'a t -> 'a t

(** [concat tt] produces the elements of each inner sequence sequentially.  If any inner
    sequences are infinite, elements of subsequent inner sequences will not be reached. *)
val concat : 'a t t -> 'a t

(** [concat_map t ~f] is [concat (map t ~f)].*)
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t

(** [concat_mapi t ~f] is like concat_map, but passes the index as an argument. *)
val concat_mapi : 'a t -> f:(int -> 'a -> 'b t) -> 'b t

(** [interleave tt] produces each element of the inner sequences of [tt] eventually, even
    if any or all of the inner sequences are infinite.  The elements of each inner
    sequence are produced in order with respect to that inner sequence.  The manner of
    interleaving among the separate inner sequences is deterministic but unspecified. *)
val interleave : 'a t t -> 'a t

(** [round_robin list] is like [interleave (of_list list)], except that the manner of
    interleaving among the inner sequences is guaranteed to be round-robin. The input
    sequences may be of different lengths; an empty sequence is dropped from subsequent
    rounds of interleaving. *)
val round_robin : 'a t list -> 'a t

(** Transforms a pair of sequences into a sequence of pairs. The length of the returned
    sequence is the length of the shorter input. The remaining elements of the longer
    input are discarded.

    WARNING: Unlike [List.zip], this will not error out if the two input sequences are of
    different lengths, because [zip] may have already returned some elements by the time
    this becomes apparent. *)
val zip : 'a t -> 'b t -> ('a * 'b) t

(** [zip_full] is like [zip], but if one sequence ends before the other, then it keeps
    producing elements from the other sequence until it has ended as well. *)
val zip_full : 'a t -> 'b t -> [ `Left of 'a | `Both of 'a * 'b | `Right of 'b ] t

(** [reduce_exn f [a1; ...; an]] is [f (... (f (f a1 a2) a3) ...) an]. It fails on the
    empty sequence. *)
val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a

val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option

(** [group l ~break] returns a sequence of lists (i.e., groups) whose concatenation is
    equal to the original sequence. Each group is broken where [break] returns true on a
    pair of successive elements.

    Example:

    {[
      group ~break:(<>) (of_list ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i']) ->

      of_list [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']] ]} *)
val group : 'a t -> break:('a -> 'a -> bool) -> 'a list t

(** [find_consecutive_duplicate t ~equal] returns the first pair of consecutive elements
    [(a1, a2)] in [t] such that [equal a1 a2].  They are returned in the same order as
    they appear in [t]. *)
val find_consecutive_duplicate : 'a t -> equal:('a -> 'a -> bool) -> ('a * 'a) option

(** The same sequence with consecutive duplicates removed.  The relative order of the
    other elements is unaffected. *)
val remove_consecutive_duplicates : 'a t -> equal:('a -> 'a -> bool) -> 'a t

(** [range ?stride ?start ?stop start_i stop_i] is the sequence of integers from [start_i]
    to [stop_i], stepping by [stride].  If [stride] < 0 then we need [start_i] > [stop_i]
    for the result to be nonempty (or [start_i] >= [stop_i] in the case where both bounds
    are inclusive). *)
val range
  :  ?stride:int (** default is [1] *)
  -> ?start:[ `inclusive | `exclusive ] (** default is [`inclusive] *)
  -> ?stop:[ `inclusive | `exclusive ] (** default is [`exclusive] *)
  -> int
  -> int
  -> int t

(** [init n ~f] is [[(f 0); (f 1); ...; (f (n-1))]].  It is an error if [n < 0]. *)
val init : int -> f:(int -> 'a) -> 'a t

(** [filter_map t ~f] produce mapped elements of [t] which are not [None]. *)
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

(** [filter_mapi] is just like [filter_map], but it also passes in the index of each
    element to [f]. *)
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t

(** [filter_opt t] produces the elements of [t] which are not [None].  [filter_opt t] =
    [filter_map t ~f:Fn.id]. *)
val filter_opt : 'a option t -> 'a t

(** [sub t ~pos ~len] is the [len]-element subsequence of [t], starting at [pos].  If the
    sequence is shorter than [pos + len], it returns [ t[pos] ... t[l-1] ], where [l] is
    the length of the sequence. *)
val sub : 'a t -> pos:int -> len:int -> 'a t

(** [take t n] produces the first [n] elements of [t]. *)
val take : 'a t -> int -> 'a t

(** [drop t n] produces all elements of [t] except the first [n] elements.  If there are
    fewer than [n] elements in [t], there is no error; the resulting sequence simply
    produces no elements.  Usually you will probably want to use [drop_eagerly] because it
    can be significantly cheaper. *)
val drop : 'a t -> int -> 'a t

(** [drop_eagerly t n] immediately consumes the first [n] elements of [t] and returns the
    unevaluated tail of [t]. *)
val drop_eagerly : 'a t -> int -> 'a t

(** [take_while t ~f] produces the longest prefix of [t] for which [f] applied to each
    element is [true]. *)
val take_while : 'a t -> f:('a -> bool) -> 'a t

(** [drop_while t ~f] produces the suffix of [t] beginning with the first element of [t]
    for which [f] is [false].  Usually you will probably want to use [drop_while_option]
    because it can be significantly cheaper. *)
val drop_while : 'a t -> f:('a -> bool) -> 'a t

(** [drop_while_option t ~f] immediately consumes the elements from [t] until the
    predicate [f] fails and returns the first element that failed along with the
    unevaluated tail of [t].  The first element is returned separately because the
    alternatives would mean forcing the consumer to evaluate the first element again (if
    the previous state of the sequence is returned) or take on extra cost for each element
    (if the element is added to the final state of the sequence using [shift_right]). *)
val drop_while_option : 'a t -> f:('a -> bool) -> ('a * 'a t) option

(** [split_n t n] immediately consumes the first [n] elements of [t] and returns the
    consumed prefix, as a list, along with the unevaluated tail of [t]. *)
val split_n : 'a t -> int -> 'a list * 'a t

(** [chunks_exn t n] produces lists of elements of [t], up to [n] elements at a time. The
    last list may contain fewer than [n] elements. No list contains zero elements. If [n]
    is not positive, it raises. *)
val chunks_exn : 'a t -> int -> 'a list t


(** [shift_right t a] produces [a] and then produces each element of [t]. *)
val shift_right : 'a t -> 'a -> 'a t

(** [shift_right_with_list t l] produces the elements of [l], then produces the elements
    of [t].  It is better to call [shift_right_with_list] with a list of size n than
    [shift_right] n times; the former will require O(1) work per element produced and the
    latter O(n) work per element produced. *)
val shift_right_with_list : 'a t -> 'a list -> 'a t

(** [shift_left t n] is a synonym for [drop t n].*)
val shift_left : 'a t -> int -> 'a t

module Infix : sig
  val ( @ ) : 'a t -> 'a t -> 'a t
end

(** Returns a sequence with all possible pairs.  The stepper function of the second
    sequence passed as argument may be applied to the same state multiple times, so be
    careful using [cartesian_product] with expensive or side-effecting functions.  If the
    second sequence is infinite, some values in the first sequence may not be reached. *)
val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

(** Returns a sequence that eventually reaches every possible pair of elements of the
    inputs, even if either or both are infinite.  The step function of both inputs may be
    applied to the same state repeatedly, so be careful using
    [interleaved_cartesian_product] with expensive or side-effecting functions. *)
val interleaved_cartesian_product : 'a t -> 'b t -> ('a * 'b) t

(** [intersperse xs ~sep] produces [sep] between adjacent elements of [xs], e.g.,
    [intersperse [1;2;3] ~sep:0 = [1;0;2;0;3]]. *)
val intersperse : 'a t -> sep:'a -> 'a t

(** [cycle_list_exn xs] repeats the elements of [xs] forever.  If [xs] is empty, it
    raises. *)
val cycle_list_exn : 'a list -> 'a t

(** [repeat a] repeats [a] forever. *)
val repeat : 'a -> 'a t

(** [singleton a] produces [a] exactly once. *)
val singleton : 'a -> 'a t

(** [delayed_fold] allows to do an on-demand fold, while maintaining a state.

    It is possible to exit early by not calling [k] in [f].  It is also possible to call
    [k] multiple times.  This results in the rest of the sequence being folded over
    multiple times, independently.

    Note that [delayed_fold], when targeting JavaScript, can result in stack overflow as
    JavaScript doesn't generally have tail call optimization. *)
val delayed_fold
  :  'a t
  -> init:'s
  -> f:('s -> 'a -> k:('s -> 'r) -> 'r) (** [k] stands for "continuation" *)
  -> finish:('s -> 'r)
  -> 'r

(** [fold_m] is a monad-friendly version of [fold]. Supply it with the monad's [return]
    and [bind], and it will chain them through the computation. *)
val fold_m
  :  bind:('acc_m -> f:('acc -> 'acc_m) -> 'acc_m)
  -> return:('acc -> 'acc_m)
  -> 'elt t
  -> init:'acc
  -> f:('acc -> 'elt -> 'acc_m)
  -> 'acc_m

(** [iter_m] is a monad-friendly version of [iter]. Supply it with the monad's [return]
    and [bind], and it will chain them through the computation. *)
val iter_m
  :  bind:('unit_m -> f:(unit -> 'unit_m) -> 'unit_m)
  -> return:(unit -> 'unit_m)
  -> 'elt t
  -> f:('elt -> 'unit_m)
  -> 'unit_m

(** [to_list_rev t] returns a list of the elements of [t], in reverse order. It is faster
    than [to_list]. *)
val to_list_rev : 'a t -> 'a list

val of_list : 'a list -> 'a t

(** [of_lazy t_lazy] produces a sequence that forces [t_lazy] the first time it needs to
    compute an element. *)
val of_lazy : 'a t Lazy.t -> 'a t

(** [memoize t] produces each element of [t], but also memoizes them so that if you
    consume the same element multiple times it is only computed once.  It's a non-eager
    version of [force_eagerly]. *)
val memoize : 'a t -> 'a t

(** [force_eagerly t] precomputes the sequence.  It is behaviorally equivalent to [of_list
    (to_list t)], but may at some point have a more efficient implementation.  It's an
    eager version of [memoize]. *)
val force_eagerly : 'a t -> 'a t

(** [bounded_length ~at_most t] returns [`Is len] if [len = length t <= at_most], and
    otherwise returns [`Greater].  Walks through only as much of the sequence as
    necessary.  Always returns [`Greater] if [at_most < 0]. *)
val bounded_length : _ t -> at_most:int -> [ `Is of int | `Greater ]

(** [length_is_bounded_by ~min ~max t] returns true if [min <= length t] and [length t <=
    max] When [min] or [max] are not provided, the check for that bound is omitted.  Walks
    through only as much of the sequence as necessary. *)
val length_is_bounded_by : ?min:int -> ?max:int -> _ t -> bool

val of_seq : 'a Caml.Seq.t -> 'a t
val to_seq : 'a t -> 'a Caml.Seq.t

(** [Generator] is a monadic interface to generate sequences in a direct style, similar to
    Python's generators.

    Here are some examples:

    {[
      open Generator

      let rec traverse_list = function
        | [] -> return ()
        | x :: xs -> yield x >>= fun () -> traverse_list xs

      let traverse_option = function
        | None -> return ()
        | Some x -> yield x

      let traverse_array arr =
        let n = Array.length arr in
        let rec loop i =
          if i >= n then return () else yield arr.(i) >>= fun () -> loop (i + 1)
        in
        loop 0

      let rec traverse_bst = function
        | Node.Empty -> return ()
        | Node.Branch (left, value, right) ->
          traverse_bst left  >>= fun () ->
          yield        value >>= fun () ->
          traverse_bst right

      let sequence_of_list   x = Generator.run (traverse_list   x)
      let sequence_of_option x = Generator.run (traverse_option x)
      let sequence_of_array  x = Generator.run (traverse_array  x)
      let sequence_of_bst    x = Generator.run (traverse_bst    x)
    ]} *)

module Generator : sig
  include Monad.S2

  val yield : 'elt -> (unit, 'elt) t
  val of_sequence : 'elt sequence -> (unit, 'elt) t
  val run : (unit, 'elt) t -> 'elt sequence
end

(** The functions in [Expert] expose internal structure which is normally meant to be
    hidden. For example, at least when [f] is purely functional, it is not intended for
    client code to distinguish between

    {[
      List.filter xs ~f
      |> Sequence.of_list
    ]}

    and

    {[
      Sequence.of_list xs
      |> Sequence.filter ~f
    ]}

    But sometimes for operational reasons it still makes sense to distinguish them. For
    example, being able to handle [Skip]s explicitly allows breaking up some
    computationally expensive sequences into smaller chunks of work. *)
module Expert : sig
  (** [next_step] returns the next step in a sequence's construction. It is like [next],
      but it also allows observing [Skip] steps. *)
  val next_step : 'a t -> ('a, 'a t) Step.t

  (** [delayed_fold_step] is liked [delayed_fold], but [f] takes an option where [None]
      represents a [Skip] step. *)
  val delayed_fold_step
    :  'a t
    -> init:'s
    -> f:('s -> 'a option -> k:('s -> 'r) -> 'r) (** [k] stands for "continuation" *)
    -> finish:('s -> 'r)
    -> 'r
end

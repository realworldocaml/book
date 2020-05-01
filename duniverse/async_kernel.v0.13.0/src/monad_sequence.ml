(** [Monad_sequence.S] is a generic interface specifying functions that deal with a
    container and a monad.  It is specialized to the [Deferred] monad and used with
    various containers in modules [Deferred.Array], [Deferred.List], [Deferred.Queue], and
    [Deferred.Sequence].  The [Monad_sequence.how] type specifies the parallelism of
    container iterators. *)

open! Core_kernel
open! Import

type how =
  [ `Parallel (** like [`Max_concurrent_jobs Int.max_value] *)
  | `Sequential (** like [`Max_concurrent_jobs 1] *)
  | `Max_concurrent_jobs of int
  ]
[@@deriving sexp_of]

module type S = sig
  type 'a monad
  type 'a t

  val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b monad) -> 'b monad
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b monad) -> 'b monad
  val find : 'a t -> f:('a -> bool monad) -> 'a option monad
  val findi : 'a t -> f:(int -> 'a -> bool monad) -> (int * 'a) option monad
  val find_map : 'a t -> f:('a -> 'b option monad) -> 'b option monad
  val find_mapi : 'a t -> f:(int -> 'a -> 'b option monad) -> 'b option monad
  val exists : 'a t -> f:('a -> bool monad) -> bool monad
  val existsi : 'a t -> f:(int -> 'a -> bool monad) -> bool monad
  val for_all : 'a t -> f:('a -> bool monad) -> bool monad
  val for_alli : 'a t -> f:(int -> 'a -> bool monad) -> bool monad
  val all : 'a monad t -> 'a t monad
  val all_unit : unit monad t -> unit monad

  (** {2 Deferred iterators}

      In the following, the default [how] is [`Sequential] *)

  val init : ?how:how -> int -> f:(int -> 'a monad) -> 'a t monad
  val iter : ?how:how -> 'a t -> f:('a -> unit monad) -> unit monad
  val iteri : ?how:how -> 'a t -> f:(int -> 'a -> unit monad) -> unit monad
  val map : ?how:how -> 'a t -> f:('a -> 'b monad) -> 'b t monad
  val mapi : ?how:how -> 'a t -> f:(int -> 'a -> 'b monad) -> 'b t monad
  val filter : ?how:how -> 'a t -> f:('a -> bool monad) -> 'a t monad
  val filteri : ?how:how -> 'a t -> f:(int -> 'a -> bool monad) -> 'a t monad
  val filter_map : ?how:how -> 'a t -> f:('a -> 'b option monad) -> 'b t monad
  val filter_mapi : ?how:how -> 'a t -> f:(int -> 'a -> 'b option monad) -> 'b t monad
  val concat_map : ?how:how -> 'a t -> f:('a -> 'b t monad) -> 'b t monad
  val concat_mapi : ?how:how -> 'a t -> f:(int -> 'a -> 'b t monad) -> 'b t monad
end

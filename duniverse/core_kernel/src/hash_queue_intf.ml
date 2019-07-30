open! Import

(** The key is used for the hashtable of queue elements. *)
module type Key = Hashtbl.Key_plain

module type S = sig
  module Key : Key

  (** A hash-queue, where the values are of type ['a]. *)
  type 'a t [@@deriving sexp_of]

  include Container.S1 with type 'a t := 'a t

  (** [invariant t] checks the invariants of the queue. *)

  val invariant : 'a t -> unit

  (** [create ()] returns an empty queue.  The arguments [growth_allowed] and [size] are
      referring to the underlying hashtable.

      @param growth_allowed defaults to true
      @param size initial size -- default to 16
  *)
  val create : ?growth_allowed:bool -> ?size:int -> unit -> 'a t

  (** Clears the queue. *)
  val clear : 'a t -> unit

  (** {2 Finding elements} *)

  (** [mem q k] returns true iff there is some (k, v) in the queue. *)
  val mem : 'a t -> Key.t -> bool

  (** [lookup t k] returns the value of the key-value pair in the queue with
      key k, if there is one. *)
  val lookup : 'a t -> Key.t -> 'a option

  val lookup_exn : 'a t -> Key.t -> 'a

  (** {2 Adding, removing, and replacing elements}

      Note that even the non-[*_exn] versions can raise, but only if there is an ongoing
      iteration. *)

  (** [enqueue t back_or_front k v] adds the key-value pair (k, v) to the front or back of
      the queue, returning [`Ok] if the pair was added, or [`Key_already_present] if there
      is already a (k, v') in the queue.
  *)
  val enqueue : 'a t -> [`back | `front] -> Key.t -> 'a -> [`Ok | `Key_already_present]

  (** Like {!enqueue}, but it raises in the [`Key_already_present] case *)
  val enqueue_exn : 'a t -> [`back | `front] -> Key.t -> 'a -> unit

  (** See {!enqueue}. [enqueue_back t k v] is the same as [enqueue t `back k v]  *)
  val enqueue_back : 'a t -> Key.t -> 'a -> [`Ok | `Key_already_present]

  (** See {!enqueue_exn}. [enqueue_back_exn t k v] is the same as [enqueue_exn t `back k v] *)
  val enqueue_back_exn : 'a t -> Key.t -> 'a -> unit

  (** See {!enqueue}. [enqueue_front t k v] is the same as [enqueue t `front k v]  *)
  val enqueue_front : 'a t -> Key.t -> 'a -> [`Ok | `Key_already_present]

  (** See {!enqueue_exn}. [enqueue_front_exn t k v] is the same as [enqueue_exn t `front k
      v] *)
  val enqueue_front_exn : 'a t -> Key.t -> 'a -> unit

  (** [lookup_and_move_to_back] finds the key-value pair (k, v) and moves it to the
      back of the queue if it exists, otherwise returning [None].

      The [_exn] versions of these functions raise if key-value pair does not exist.
  *)
  val lookup_and_move_to_back : 'a t -> Key.t -> 'a option

  (** Like {!lookup_and_move_to_back}, but raises instead of returning an option *)
  val lookup_and_move_to_back_exn : 'a t -> Key.t -> 'a

  (** Like {!lookup_and_move_to_back}, but moves element to the front of the queue *)
  val lookup_and_move_to_front : 'a t -> Key.t -> 'a option

  (** Like {!lookup_and_move_to_front}, but raises instead of returning an option *)
  val lookup_and_move_to_front_exn : 'a t -> Key.t -> 'a

  (** [first t] returns the front element of the queue, without removing it. *)
  val first : 'a t -> 'a option

  (** [first_with_key t] returns the front element of the queue and its key, without
      removing it. *)
  val first_with_key : 'a t -> (Key.t * 'a) option

  (** [keys t] returns the keys in the order of the queue. *)
  val keys : 'a t -> Key.t list

  (** [dequeue t front_or_back] returns the front or back element of the queue. *)
  val dequeue : 'a t -> [`back | `front] -> 'a option

  (** Like {!dequeue}, but it raises if the queue is empty. *)
  val dequeue_exn : 'a t -> [`back | `front] -> 'a

  (** [dequeue_back t] returns the back element of the queue. *)
  val dequeue_back : 'a t -> 'a option

  (** Like {!dequeue_back}, but it raises if the queue is empty. *)
  val dequeue_back_exn : 'a t -> 'a

  (** [dequeue_front t] returns the front element of the queue. *)
  val dequeue_front : 'a t -> 'a option

  (** Like {!dequeue_front}, but it raises if the queue is empty. *)
  val dequeue_front_exn : 'a t -> 'a

  (** [dequeue_with_key t] returns the front or back element of the queue and its key. *)
  val dequeue_with_key : 'a t -> [`back | `front] -> (Key.t * 'a) option

  (** Like {!dequeue_with_key}, but it raises if the queue is empty. *)
  val dequeue_with_key_exn : 'a t -> [`back | `front] -> Key.t * 'a

  (** [dequeue_back_with_key t] returns the back element of the queue and its key. *)
  val dequeue_back_with_key : 'a t -> (Key.t * 'a) option

  (** Like {!dequeue_back_with_key}, but it raises if the queue is empty. *)
  val dequeue_back_with_key_exn : 'a t -> Key.t * 'a

  (** [dequeue_front_with_key t] returns the front element of the queue and its key. *)
  val dequeue_front_with_key : 'a t -> (Key.t * 'a) option

  (** Like {!dequeue_front_with_key}, but it raises if the queue is empty. *)
  val dequeue_front_with_key_exn : 'a t -> Key.t * 'a

  (** [dequeue_all t ~f] dequeues every element of the queue and applies [f] to each one.
      The dequeue order is from front to back. *)
  val dequeue_all : 'a t -> f:('a -> unit) -> unit

  (** [remove q k] removes the key-value pair with key [k] from the queue. *)
  val remove : 'a t -> Key.t -> [`Ok | `No_such_key]

  val remove_exn : 'a t -> Key.t -> unit

  (** [replace q k v] changes the value of key [k] in the queue to [v]. *)
  val replace : 'a t -> Key.t -> 'a -> [`Ok | `No_such_key]

  val replace_exn : 'a t -> Key.t -> 'a -> unit

  (** [drop ?n q back_or_front] drops [n] elements (default 1) from the back or front of
      the queue. If the queue has fewer than [n] elements then it is cleared. *)
  val drop : ?n:int -> 'a t -> [`back | `front] -> unit

  (** Equivalent to [drop ?n q `front]. *)
  val drop_front : ?n:int -> 'a t -> unit

  (** Equivalent to [drop ?n q `back]. *)
  val drop_back : ?n:int -> 'a t -> unit

  (** {2 Iterating over elements} *)

  (** [iter t ~f] applies [f] to each key and element of the queue. *)
  val iteri : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit

  val foldi : 'a t -> init:'b -> f:('b -> key:Key.t -> data:'a -> 'b) -> 'b
end

open! Import

(** Signatures required of types which can be used in [[@@deriving hash]]. *)


module type S = sig
  (** The type that is hashed.  *)
  type t

  (** [hash_fold_t state x] mixes the content of [x] into the [state].

      By default, all our [hash_fold_t] functions (derived or not) should satisfy the
      following properties.

      1. [hash_fold_t state x] should mix all the information present in [x] in the state.
      That is, by default, [hash_fold_t] will traverse the full term [x] (this is a
      significant change for Hashtbl.hash which by default stops traversing the term after
      after considering a small number of "significant values"). [hash_fold_t] must not
      discard the [state].

      2. [hash_fold_t] must be compatible with the associated [compare] function: that is,
      for all [x] [y] and [s], [compare x y = 0] must imply [hash_fold_t s x = hash_fold_t
      s y].

      3. To avoid avoid systematic collisions, [hash_fold_t] should expand to different
      sequences of built-in mixing functions for different values of [x]. No such sequence
      is allowed to be a prefix of another.

      A common mistake is to implement [hash_fold_t] of a collection by just folding all
      the elements. This makes the folding sequence of [a] be a prefix of [a @ b], thereby
      violating the requirement. This creates large families of collisions: all of the
      following collections would hash the same:

      {v
      [[]; [1;2;3]]
      [[1]; [2;3]]
      [[1; 2]; [3]]
      [[1; 2; 3]; []]
      [[1]; [2]; []; [3];]
      ...
      v}

      A good way to avoid this is to mix in the size of the collection to the beginning
      ([fold ~init:(hash_fold_int state length) ~f:hash_fold_elem]). The default in our
      libraries is to mix the length of the structure before folding. To prevent the
      aforementioned collisions, one should respect this ordering.
  *)
  val hash_fold_t : Hash.state -> t -> Hash.state
end

module type S1 = sig
  type 'a t

  val hash_fold_t : (Hash.state -> 'a -> Hash.state) -> Hash.state -> 'a t -> Hash.state
end

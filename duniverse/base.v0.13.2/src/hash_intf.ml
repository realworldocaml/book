(** [Hash_intf.S] is the interface which a hash function must support.

    The functions of [Hash_intf.S] are only allowed to be used in specific sequence:

    [alloc], [reset ?seed], [fold_..*], [get_hash_value], [reset ?seed], [fold_..*],
    [get_hash_value], ...

    (The optional [seed]s passed to each reset may differ.)

    The chain of applications from [reset] to [get_hash_value] must be done in a
    single-threaded manner (you can't use [fold_*] on a state that's been used
    before). More precisely, [alloc ()] creates a new family of states. All functions that
    take [t] and produce [t] return a new state from the same family.

    At any point in time, at most one state in the family is "valid". The other states are
    "invalid".

    - The state returned by [alloc] is invalid.
    - The state returned by [reset] is valid (all of the other states become invalid).
    - The [fold_*] family of functions requires a valid state and produces a valid state
      (thereby making the input state invalid).
    - [get_hash_value] requires a valid state and makes it invalid.

    These requirements are currently formally encoded in the [Check_initialized_correctly]
    module in bench/bench.ml. *)

open! Import0

module type S = sig
  (** Name of the hash-function, e.g., "internalhash", "siphash" *)
  val description : string

  (** [state] is the internal hash-state used by the hash function. *)
  type state

  (** [fold_<T> state v] incorporates a value [v] of type <T> into the hash-state,
      returning a modified hash-state.  Implementations of the [fold_<T>] functions may
      mutate the [state] argument in place, and return a reference to it.  Implementations
      of the fold_<T> functions should not allocate. *)
  val fold_int : state -> int -> state

  val fold_int64 : state -> int64 -> state
  val fold_float : state -> float -> state
  val fold_string : state -> string -> state

  (** [seed] is the type used to seed the initial hash-state. *)
  type seed

  (** [alloc ()] returns a fresh uninitialized hash-state.  May allocate. *)
  val alloc : unit -> state

  (** [reset ?seed state] initializes/resets a hash-state with the given [seed], or else a
      default-seed. Argument [state] may be mutated. Should not allocate. *)
  val reset : ?seed:seed -> state -> state

  (** [hash_value] The type of hash values, returned by [get_hash_value]. *)
  type hash_value

  (** [get_hash_value] extracts a hash-value from the hash-state. *)
  val get_hash_value : state -> hash_value

  module For_tests : sig
    val compare_state : state -> state -> int
    val state_to_string : state -> string
  end
end

module type Builtin_hash_fold_intf = sig
  type state
  type 'a folder = state -> 'a -> state

  val hash_fold_nativeint : nativeint folder
  val hash_fold_int64 : int64 folder
  val hash_fold_int32 : int32 folder
  val hash_fold_char : char folder
  val hash_fold_int : int folder
  val hash_fold_bool : bool folder
  val hash_fold_string : string folder
  val hash_fold_float : float folder
  val hash_fold_unit : unit folder
  val hash_fold_option : 'a folder -> 'a option folder
  val hash_fold_list : 'a folder -> 'a list folder
  val hash_fold_lazy_t : 'a folder -> 'a lazy_t folder

  (** Hash support for [array] and [ref] is provided, but is potentially DANGEROUS, since
      it incorporates the current contents of the array/ref into the hash value.  Because
      of this we add a [_frozen] suffix to the function name.

      Hash support for [string] is also potentially DANGEROUS, but strings are mutated
      less often, so we don't append [_frozen] to it.

      Also note that we don't support [bytes]. *)
  val hash_fold_ref_frozen : 'a folder -> 'a ref folder

  val hash_fold_array_frozen : 'a folder -> 'a array folder
end

module type Builtin_hash_intf = sig
  type hash_value

  val hash_nativeint : nativeint -> hash_value
  val hash_int64 : int64 -> hash_value
  val hash_int32 : int32 -> hash_value
  val hash_char : char -> hash_value
  val hash_int : int -> hash_value
  val hash_bool : bool -> hash_value
  val hash_string : string -> hash_value
  val hash_float : float -> hash_value
  val hash_unit : unit -> hash_value
end

module type Builtin_intf = sig
  include Builtin_hash_fold_intf
  include Builtin_hash_intf
end

module type Full = sig
  include S (** @inline *)

  type 'a folder = state -> 'a -> state

  (** [create ?seed ()] is a convenience.  Equivalent to [reset ?seed (alloc ())]. *)
  val create : ?seed:seed -> unit -> state

  (** [of_fold fold] constructs a standard hash function from an existing fold
      function. *)
  val of_fold : (state -> 'a -> state) -> 'a -> hash_value

  module Builtin :
    Builtin_intf
    with type state := state
     and type 'a folder := 'a folder
     and type hash_value := hash_value

  (** [run ?seed folder x] runs [folder] on [x] in a newly allocated hash-state,
      initialized using optional [seed] or a default-seed.

      The following identity exists: [run [%hash_fold: T]] == [[%hash: T]]

      [run] can be used if we wish to run a hash-folder with a non-default seed. *)
  val run : ?seed:seed -> 'a folder -> 'a -> hash_value
end

module type Hash = sig
  module type Full = Full
  module type S = S

  module F (Hash : S) :
    Full
    with type hash_value = Hash.hash_value
     and type state = Hash.state
     and type seed = Hash.seed

  (** The code of [ppx_hash] is agnostic to the choice of hash algorithm that is
      used. However, it is not currently possible to mix various choices of hash algorithms
      in a given code base.

      We experimented with:
      - (a) custom hash algorithms implemented in OCaml and
      - (b) in C;
      - (c) OCaml's internal hash function (which is a custom version of Murmur3,
        implemented in C);
      - (d) siphash, a modern hash function implemented in C.

      Our findings were as follows:

      - Implementing our own custom hash algorithms in OCaml and C yielded very little
        performance improvement over the (c) proposal, without providing the benefit of being
        a peer-reviewed, widely used hash function.

      - Siphash (a modern hash function with an internal state of 32 bytes) has a worse
        performance profile than (a,b,c) above (hashing takes more time). Since its internal
        state is bigger than an OCaml immediate value, one must either manage allocation of
        such state explicitly, or paying the cost of allocation each time a hash is computed.
        While being a supposedly good hash function (with good hash quality), this quality was
        not translated in measurable improvements in our macro benchmarks. (Also, based on
        the data available at the time of writing, it's unclear that other hash algorithms in
        this class would be more than marginally faster.)

      - By contrast, using the internal combinators of OCaml hash function means that we do
        not allocate (the internal state of this hash function is 32 bit) and have the same
        quality and performance as Hashtbl.hash.

      Hence, we are here making the choice of using this Internalhash (that is, Murmur3, the
      OCaml hash algorithm as of 4.03) as our hash algorithm. It means that the state of the
      hash function does not need to be preallocated, and makes for simpler use in hash
      tables and other structures. *)

  (** @open *)
  include
    Full
    with type state = Base_internalhash_types.state
     and type seed = Base_internalhash_types.seed
     and type hash_value = Base_internalhash_types.hash_value
end

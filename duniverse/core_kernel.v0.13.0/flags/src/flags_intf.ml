(** [module Flags] implements Unix-style sets of flags that are represented as an [int]
    with various bits set, one bit for each flag.  E.g. [Linux_ext.Epoll.Flag].

    [Flags] defines a module type [Flags.S], the interface for a flags, and a functor
    [Flags.Make] for creating a flags implementation. *)

open! Core_kernel

(** [module type S] is the interface for a set of flags.  Values of [type t] are set of
    flags, and the various functions operate on sets of flags.  There is a finite universe
    of flags (in particular 63 flags, one for each bit).

    [sexp_of_t] and [t_of_sexp] use the flag names supplied to [Flags.Make]. *)
module type S = sig
  type t [@@deriving sexp, typerep]

  (** consistent with subset *)
  include Comparable.S with type t := t

  val of_int : int -> t
  val to_int_exn : t -> int
  val empty : t

  (** set union, bitwise or *)
  val ( + ) : t -> t -> t

  (** set difference.  Although we use operators [+] and [-], they do not satisfy the
      usual arithmetic equations, e.g. [x - y = x + (empty - y)] does not hold. *)
  val ( - ) : t -> t -> t

  (** bitwise and *)
  val intersect : t -> t -> t

  (** bitwise not *)
  val complement : t -> t

  val is_empty : t -> bool
  val do_intersect : t -> t -> bool
  val are_disjoint : t -> t -> bool

  (** [is_subset t ~of_] is [t = intersect t of_] *)
  val is_subset : t -> of_:t -> bool

  module Unstable : sig
    type nonrec t = t [@@deriving bin_io, compare, sexp]
  end
end

module type Make_arg = sig
  (** An entry [flag, name] in [known] means that the bit(s) in [flag] is (are) called
      [name]; i.e. if [bit_and flags flag = flag], then the bit(s) is (are) set and [name]
      will appear in [sexp_of_t flags].  [known] is only used to make [sexp_of_t]'s output
      human readable.

      The flags in the output of [sexp_of_t] will occur in the same order as they appear
      in [known].

      It is allowed to have a single flag with multiple bits set.

      It is an error if different flags intersect, and [allow_intersecting = false]. *)
  val known : (Int63.t * string) list

  (** If [remove_zero_flags], then all flags with value zero will be automatically removed
      from [known].  If [not remove_zero_flags], then it is an error for [known] to contain
      any flags with value zero.

      About this existence of this option: it seems better to make it an option here
      rather than do the filtering at the functor call site.  It also makes clear to
      callers that they need to think about zero flags, and clear what they can do if they
      encounter them. *)
  val remove_zero_flags : bool

  (** [allow_intersecting] says whether to allow intersecting [known] flags.  It is
      common to do [allow_intersecting = false], however in some situations, e.g.
      Unix open flags, the flags intersect. *)
  val allow_intersecting : bool

  (** [should_print_error] says whether to print an error message if there is an error in
      the known flags.  It is typical to use [should_print_error = true] because
      [Flags.Make] is applied at the module level, where the exception raised isn't
      displayed nicely. *)
  val should_print_error : bool
end

module type Flags = sig
  module type Make_arg = Make_arg
  module type S = S

  (** [create ~bit:n] creates a flag with the [n]th bit set.  [n] must be between 0 and
      62.

      Typically a flag has one bit set; [create] is useful in exactly those cases.  For
      flags with multiple bits one can either define the Int63.t directly or create it in
      terms of simpler flags, using [+] and [-]. *)
  val create : bit:int -> Int63.t

  (** [Flags.Make] builds a new flags module.  If there is an error in the [known] flags,
      it behaves as per [on_error].

      We expose [type t = int] in the result of [Flags.Make] so that one can easily use
      flag constants as values of the flag type without having to coerce them.  It is
      typical to hide the [t = int] in another signature [S]. *)
  module Make (M : Make_arg) : S with type t = Int63.t
end

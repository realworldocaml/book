open! Import
module Key = Hashtbl_intf.Key

module type Accessors = sig
  include Container.Generic

  (** override [Container.Generic.mem] *)
  val mem : 'a t -> 'a -> bool

  (** preserves the equality function *)
  val copy : 'a t -> 'a t

  val add : 'a t -> 'a -> unit

  (** [strict_add t x] returns [Ok ()] if the [x] was not in [t], or an [Error] if it
      was. *)
  val strict_add : 'a t -> 'a -> unit Or_error.t

  val strict_add_exn : 'a t -> 'a -> unit
  val remove : 'a t -> 'a -> unit

  (** [strict_remove t x] returns [Ok ()] if the [x] was in [t], or an [Error] if it
      was not. *)
  val strict_remove : 'a t -> 'a -> unit Or_error.t

  val strict_remove_exn : 'a t -> 'a -> unit
  val clear : 'a t -> unit
  val equal : 'a t -> 'a t -> bool
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val filter_inplace : 'a t -> f:('a -> bool) -> unit

  (** [inter t1 t2] computes the set intersection of [t1] and [t2].  Runs in O(min(length
      t1, length t2)).  Behavior is undefined if [t1] and [t2] don't have the same
      equality function. *)
  val inter : 'key t -> 'key t -> 'key t

  val union : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val of_hashtbl_keys : ('a, _) Hashtbl.t -> 'a t
  val to_hashtbl : 'key t -> f:('key -> 'data) -> ('key, 'data) Hashtbl.t
end

type ('key, 'z) create_options = ('key, unit, 'z) Hashtbl_intf.create_options

type ('key, 'z) create_options_without_first_class_module =
  ('key, unit, 'z) Hashtbl_intf.create_options_without_first_class_module

module type Creators = sig
  type 'a t

  val create
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> 'a t

  val of_list
    :  ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'a Key.t
    -> 'a list
    -> 'a t
end

module type Creators_generic = sig
  type 'a t
  type 'a elt
  type ('a, 'z) create_options

  val create : ('a, unit -> 'a t) create_options
  val of_list : ('a, 'a elt list -> 'a t) create_options
end

module Check = struct
  module Make_creators_check
      (Type : T.T1)
      (Elt : T.T1)
      (Options : T.T2)
      (M : Creators_generic
       with type 'a t := 'a Type.t
       with type 'a elt := 'a Elt.t
       with type ('a, 'z) create_options := ('a, 'z) Options.t) =
  struct end

  module Check_creators_is_specialization_of_creators_generic (M : Creators) =
    Make_creators_check
      (struct
        type 'a t = 'a M.t
      end)
      (struct
        type 'a t = 'a
      end)
      (struct
        type ('a, 'z) t = ('a, 'z) create_options
      end)
      (struct
        include M

        let create ?growth_allowed ?size m () = create ?growth_allowed ?size m
      end)
end

module type Hash_set = sig
  type 'a t [@@deriving_inline sexp_of]

  val sexp_of_t : ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t

  [@@@end]

  (** We use [[@@deriving sexp_of]] but not [[@@deriving sexp]] because we want people to be
      explicit about the hash and comparison functions used when creating hashtables.  One
      can use [Hash_set.Poly.t], which does have [[@@deriving sexp]], to use polymorphic
      comparison and hashing. *)

  module Key = Key

  module type Creators = Creators
  module type Creators_generic = Creators_generic

  type nonrec ('key, 'z) create_options = ('key, 'z) create_options

  include Creators with type 'a t := 'a t (** @open *)

  module type Accessors = Accessors

  include Accessors with type 'a t := 'a t with type 'a elt = 'a (** @open *)

  val hashable_s : 'key t -> 'key Key.t

  type nonrec ('key, 'z) create_options_without_first_class_module =
    ('key, 'z) create_options_without_first_class_module

  (** A hash set that uses polymorphic comparison *)
  module Poly : sig
    type nonrec 'a t = 'a t [@@deriving_inline sexp]

    include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t := 'a t

    [@@@end]

    include
      Creators_generic
      with type 'a t := 'a t
      with type 'a elt = 'a
      with type ('key, 'z) create_options :=
        ('key, 'z) create_options_without_first_class_module

    include Accessors with type 'a t := 'a t with type 'a elt := 'a elt
  end

  (** [M] is meant to be used in combination with OCaml applicative functor types:

      {[
        type string_hash_set = Hash_set.M(String).t
      ]}

      which stands for:

      {[
        type string_hash_set = (String.t, int) Hash_set.t
      ]}

      The point is that [Hash_set.M(String).t] supports deriving, whereas the second
      syntax doesn't (because [t_of_sexp] doesn't know what comparison/hash function to
      use). *)
  module M (Elt : T.T) : sig
    type nonrec t = Elt.t t
  end

  module type Sexp_of_m = sig
    type t [@@deriving_inline sexp_of]

    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

    [@@@end]
  end

  module type M_of_sexp = sig
    type t [@@deriving_inline of_sexp]

    val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t

    [@@@end]

    include Hashtbl_intf.Key.S with type t := t
  end

  val sexp_of_m__t : (module Sexp_of_m with type t = 'elt) -> 'elt t -> Sexp.t
  val m__t_of_sexp : (module M_of_sexp with type t = 'elt) -> Sexp.t -> 'elt t

  module Creators (Elt : sig
      type 'a t

      val hashable : 'a t Hashable.t
    end) : sig
    type 'a t_ = 'a Elt.t t

    val t_of_sexp : (Sexp.t -> 'a Elt.t) -> Sexp.t -> 'a t_

    include
      Creators_generic
      with type 'a t := 'a t_
      with type 'a elt := 'a Elt.t
      with type ('elt, 'z) create_options :=
        ('elt, 'z) create_options_without_first_class_module
  end

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    val hashable : 'a t -> 'a Hashable.t
  end
end

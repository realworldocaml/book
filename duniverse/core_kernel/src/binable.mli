(** Module types and utilities for dealing with types that support the bin-io binary
    encoding. *)

open! Import
open Bin_prot.Binable
open Bigarray

(** We copy the definition of the bigstring type here, because we cannot depend on
    bigstring.ml *)
type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

(** New code should use [@@deriving bin_io].  These module types ([S], [S1], and [S2]) are
    exported only for backwards compatibility. *)
module type S = S

module type S_only_functions = S_only_functions
module type S1 = S1
module type S2 = S2
module type S3 = S3

module Minimal : sig
  module type S = Minimal.S
  module type S1 = Minimal.S1
  module type S2 = Minimal.S2
  module type S3 = Minimal.S3
end

(** [Of_binable*] functors are for when you want the binary representation of one type to
    be the same as that for some other isomorphic type. *)

module Of_binable
    (Binable : Minimal.S) (M : sig
                             type t

                             val to_binable : t -> Binable.t
                             val of_binable : Binable.t -> t
                           end) : S with type t := M.t

module Of_binable1
    (Binable : Minimal.S1) (M : sig
                              type 'a t

                              val to_binable : 'a t -> 'a Binable.t
                              val of_binable : 'a Binable.t -> 'a t
                            end) : S1 with type 'a t := 'a M.t

module Of_binable2
    (Binable : Minimal.S2) (M : sig
                              type ('a, 'b) t

                              val to_binable : ('a, 'b) t -> ('a, 'b) Binable.t
                              val of_binable : ('a, 'b) Binable.t -> ('a, 'b) t
                            end) : S2 with type ('a, 'b) t := ('a, 'b) M.t

module Of_binable3
    (Binable : Minimal.S3) (M : sig
                              type ('a, 'b, 'c) t

                              val to_binable : ('a, 'b, 'c) t -> ('a, 'b, 'c) Binable.t
                              val of_binable : ('a, 'b, 'c) Binable.t -> ('a, 'b, 'c) t
                            end) : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t

(** [Of_sexpable] serializes a value using the bin-io of the sexp serialization of the
    value.  This is not as efficient as using [@@deriving bin_io]. However, it is useful
    when performance isn't important and there are obstacles to using [@@deriving bin_io],
    e.g., some type missing [@@deriving bin_io]. [Of_sexpable] is also useful when one
    wants to be forgiving about format changes, due to the sexp serialization being more
    robust to changes like adding or removing a constructor. *)
module Of_sexpable (M : Sexpable.S) : S with type t := M.t

module Of_stringable (M : Stringable.S) : S with type t := M.t

type 'a m = (module S with type t = 'a)

val of_bigstring : 'a m -> bigstring -> 'a

val to_bigstring
  :  ?prefix_with_length:bool (** defaults to false *)
  -> 'a m
  -> 'a
  -> bigstring

val of_string : 'a m -> string -> 'a
val to_string : 'a m -> 'a -> string

(** The following functors preserve stability: if applied to stable types with stable
    (de)serializations, they will produce stable types with stable (de)serializations.

    Note: In all cases, stability of the input (and therefore the output) depends on the
    semantics of all conversion functions (e.g. [to_string], [to_sexpable]) not changing
    in the future.
*)

module Stable : sig
  module Of_binable : sig
    module V1 : module type of Of_binable
  end

  module Of_binable1 : sig
    module V1 : module type of Of_binable1
  end

  module Of_binable2 : sig
    module V1 : module type of Of_binable2
  end

  module Of_binable3 : sig
    module V1 : module type of Of_binable3
  end

  module Of_sexpable : sig
    module V1 : module type of Of_sexpable
  end

  module Of_stringable : sig
    module V1 : module type of Of_stringable
  end
end

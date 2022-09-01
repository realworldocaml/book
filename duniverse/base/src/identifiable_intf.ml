(** A signature combining functionality that is commonly used for types that are intended
    to act as names or identifiers.

    Modules that satisfy [Identifiable] can be printed and parsed (both through string and
    s-expression converters) and can be used in hash-based and comparison-based
    containers (e.g., hashtables and maps).

    This module also provides functors for conveniently constructing identifiable
    modules. *)

open! Import

module type Arg = sig
  type t [@@deriving_inline compare, hash, sexp]

  include Ppx_compare_lib.Comparable.S with type t := t
  include Ppx_hash_lib.Hashable.S with type t := t
  include Sexplib0.Sexpable.S with type t := t

  [@@@end]

  include Stringable.S with type t := t

  (** For registering the pretty printer. *)
  val module_name : string
end

module type Arg_with_comparator = sig
  include Arg
  include Comparator.S with type t := t
end

module type S = sig
  type t [@@deriving_inline hash, sexp]

  include Ppx_hash_lib.Hashable.S with type t := t
  include Sexplib0.Sexpable.S with type t := t

  [@@@end]

  include Stringable.S with type t := t
  include Comparable.S with type t := t
  include Pretty_printer.S with type t := t

  val hashable : t Hashable.t
end

module type Identifiable = sig
  module type Arg = Arg
  module type Arg_with_comparator = Arg_with_comparator
  module type S = S

  (** Used for making an Identifiable module.  Here's an example.

      {[
        module Id = struct
          module T = struct
            type t = A | B [@@deriving compare, hash, sexp]
            let of_string s = t_of_sexp (sexp_of_string s)
            let to_string t = string_of_sexp (sexp_of_t t)
            let module_name = "My_library.Id"
          end
          include T
          include Identifiable.Make (T)
        end
      ]} *)
  module Make (M : Arg) : S with type t := M.t

  module Make_using_comparator (M : Arg_with_comparator) :
    S with type t := M.t with type comparator_witness := M.comparator_witness
end

open! Import

module type S = sig
  type t [@@deriving_inline hash, sexp]

  val hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
  val hash : t -> Ppx_hash_lib.Std.Hash.hash_value

  include Ppx_sexp_conv_lib.Sexpable.S with type t := t

  [@@@end]

  include Stringable.S with type t := t
  include Comparable.S with type t := t
  include Pretty_printer.S with type t := t
end

module Make (T : sig
    type t [@@deriving_inline compare, hash, sexp]

    val compare : t -> t -> int
    val hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
    val hash : t -> Ppx_hash_lib.Std.Hash.hash_value

    include Ppx_sexp_conv_lib.Sexpable.S with type t := t

    [@@@end]

    include Stringable.S with type t := t

    val module_name : string
  end) =
struct
  include T
  include Comparable.Make (T)
  include Pretty_printer.Register (T)
end

module Make_using_comparator (T : sig
    type t [@@deriving_inline hash, sexp]

    val hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
    val hash : t -> Ppx_hash_lib.Std.Hash.hash_value

    include Ppx_sexp_conv_lib.Sexpable.S with type t := t

    [@@@end]

    include Comparator.S with type t := t
    include Stringable.S with type t := t

    val module_name : string
  end) =
struct
  include T
  include Comparable.Make_using_comparator (T)
  include Pretty_printer.Register (T)
end

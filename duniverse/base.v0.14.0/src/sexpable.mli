(** Provides functors for making modules sexpable. New code should use the [[@@deriving
    sexp]] syntax directly.  These module types ([S], [S1], [S2], and [S3]) are exported
    for backwards compatibility only. *)

open! Import

include module type of Sexplib0.Sexpable (** @inline *)

(** For when you want the sexp representation of one type to be the same as that for
    some other isomorphic type. *)
module Of_sexpable
    (Sexpable : S) (M : sig
                      type t

                      val to_sexpable : t -> Sexpable.t
                      val of_sexpable : Sexpable.t -> t
                    end) : S with type t := M.t

module Of_sexpable1
    (Sexpable : S1) (M : sig
                       type 'a t

                       val to_sexpable : 'a t -> 'a Sexpable.t
                       val of_sexpable : 'a Sexpable.t -> 'a t
                     end) : S1 with type 'a t := 'a M.t

module Of_sexpable2
    (Sexpable : S2) (M : sig
                       type ('a, 'b) t

                       val to_sexpable : ('a, 'b) t -> ('a, 'b) Sexpable.t
                       val of_sexpable : ('a, 'b) Sexpable.t -> ('a, 'b) t
                     end) : S2 with type ('a, 'b) t := ('a, 'b) M.t

module Of_sexpable3
    (Sexpable : S3) (M : sig
                       type ('a, 'b, 'c) t

                       val to_sexpable : ('a, 'b, 'c) t -> ('a, 'b, 'c) Sexpable.t
                       val of_sexpable : ('a, 'b, 'c) Sexpable.t -> ('a, 'b, 'c) t
                     end) : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t

module Of_stringable (M : Stringable.S) : S with type t := M.t

open! Import

module type Key = sig
  type t [@@deriving_inline compare, sexp_of]

  val compare : t -> t -> int
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

  [@@@end]

  (** Values returned by [hash] must be non-negative.  An exception will be raised in the
      case that [hash] returns a negative value. *)
  val hash : t -> int
end

module Hashable = struct
  type 'a t =
    { hash : 'a -> int
    ; compare : 'a -> 'a -> int
    ; sexp_of_t : 'a -> Sexp.t
    }

  (** This function is sound but not complete, meaning that if it returns [true] then it's
      safe to use the two interchangeably.  If it's [false], you have no guarantees.  For
      example:

      {[
        > utop
        open Core;;
        let equal (a : 'a Hashtbl_intf.Hashable.t) b =
          phys_equal a b
          || (phys_equal a.hash b.hash
              && phys_equal a.compare b.compare
              && phys_equal a.sexp_of_t b.sexp_of_t)
        ;;
        let a = Hashtbl_intf.Hashable.{ hash; compare; sexp_of_t = Int.sexp_of_t };;
        let b = Hashtbl_intf.Hashable.{ hash; compare; sexp_of_t = Int.sexp_of_t };;
        equal a b;;  (* false?! *)
      ]}
  *)
  let equal a b =
    phys_equal a b
    || (phys_equal a.hash b.hash
        && phys_equal a.compare b.compare
        && phys_equal a.sexp_of_t b.sexp_of_t)
  ;;

  let hash_param = Caml.Hashtbl.hash_param
  let hash = Caml.Hashtbl.hash
  let poly = { hash; compare = Poly.compare; sexp_of_t = (fun _ -> Sexp.Atom "_") }

  let of_key (type a) (module Key : Key with type t = a) =
    { hash = Key.hash; compare = Key.compare; sexp_of_t = Key.sexp_of_t }
  ;;

  let to_key (type a) { hash; compare; sexp_of_t } =
    (module struct
      type t = a

      let hash = hash
      let compare = compare
      let sexp_of_t = sexp_of_t
    end : Key
      with type t = a)
  ;;
end

include Hashable

module type Hashable = sig
  type 'a t = 'a Hashable.t =
    { hash : 'a -> int
    ; compare : 'a -> 'a -> int
    ; sexp_of_t : 'a -> Sexp.t
    }

  val equal : 'a t -> 'a t -> bool
  val poly : 'a t
  val of_key : (module Key with type t = 'a) -> 'a t
  val to_key : 'a t -> (module Key with type t = 'a)
  val hash_param : int -> int -> 'a -> int
  val hash : 'a -> int
end

open! Core_kernel
open! Import

type ('a, 'b) t = { all : 'a list }

module type S =
  Enumeration_intf.S with type ('a, 'witness) enumeration := ('a, 'witness) t

module type S_fc =
  Enumeration_intf.S_fc with type ('a, 'witness) enumeration := ('a, 'witness) t

module Make (T : sig
    type t [@@deriving enumerate]
  end) =
struct
  type enumeration_witness

  let enumeration = T.{ all }
end

let make (type t) ~all =
  (module struct
    type enumerable_t = t
    type enumeration_witness

    let enumeration = { all }
  end : S_fc
    with type enumerable_t = t)
;;

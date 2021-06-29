open! Core_kernel
open! Import

module type S = sig
  type ('a, 'b) enumeration
  type t
  type enumeration_witness

  val enumeration : (t, enumeration_witness) enumeration
end

(* "fc" stands for "first class" *)
module type S_fc = sig
  type enumerable_t

  include S with type t := enumerable_t
end

module type Enumeration = sig
  type ('a, 'witness) t = private { all : 'a list }

  module type S = S with type ('a, 'witness) enumeration := ('a, 'witness) t
  module type S_fc = S_fc with type ('a, 'witness) enumeration := ('a, 'witness) t

  module Make (T : sig
      type t [@@deriving enumerate]
    end) : S with type t := T.t

  val make : all:'a list -> (module S_fc with type enumerable_t = 'a)
end

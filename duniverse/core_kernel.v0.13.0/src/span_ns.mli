open! Import
open Std_internal
include Time_ns_intf.Span

module Stable : sig
  (** [V1] is currently only implemented in [Core]. *)
  module V2 : sig
    type nonrec t = t [@@deriving hash]
    type nonrec comparator_witness = comparator_witness

    include
      Stable_int63able
      with type t := t
      with type comparator_witness := comparator_witness

    include
      Comparable.Stable.V1.S
      with type comparable := t
      with type comparator_witness := comparator_witness
  end
end

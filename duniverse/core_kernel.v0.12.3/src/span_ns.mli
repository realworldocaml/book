open! Import
open Std_internal
include Time_ns_intf.Span

module Stable : sig
  (** [V1] is currently only implemented in [Core]. *)
  module V2 : sig
    type nonrec t = t [@@deriving hash]

    include Stable_int63able with type t := t
  end
end

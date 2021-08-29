(** This module extends {{!Base.Source_code_position}[Base.Source_code_position]}. *)

(** @open *)
include module type of struct
  include Base.Source_code_position
end

include Comparable.S with type t := t and type comparator_witness := comparator_witness
include Hashable.S with type t := t

module Stable : sig
  module V1 : Stable_module_types.S0 with type t = t
end

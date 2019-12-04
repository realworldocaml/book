open! Import
include Base.Source_code_position

include (
  Source_code_position0 :
    module type of Source_code_position0
  with type t := t
  with type comparator_witness := comparator_witness)

include Comparable.Extend (Base.Source_code_position) (Source_code_position0)
include Hashable.Make (Source_code_position0)

open! Import

module type Infix = Base.Comparable.Infix
module type Polymorphic_compare = Base.Comparable.Polymorphic_compare
module type Validate = Base.Comparable.Validate
module type With_zero = Base.Comparable.With_zero

module type S_common = sig
  include Base.Comparable.S
  module Replace_polymorphic_compare : Polymorphic_compare with type t := t
end

(** Usage example:

    {[
      module Foo : sig
        type t = ...
        include Comparable.S with type t := t
      end
    ]}

    Then use [Comparable.Make] in the struct (see comparable.mli for an example). *)

module type S_plain = sig
  include S_common

  module Map :
    Map.S_plain with type Key.t = t with type Key.comparator_witness = comparator_witness

  module Set :
    Set.S_plain with type Elt.t = t with type Elt.comparator_witness = comparator_witness
end

module type S = sig
  include S_common

  module Map :
    Map.S with type Key.t = t with type Key.comparator_witness = comparator_witness

  module Set :
    Set.S with type Elt.t = t with type Elt.comparator_witness = comparator_witness
end

module type Map_and_set_binable = sig
  type t

  include Comparator.S with type t := t

  module Map :
    Map.S_binable
    with type Key.t = t
    with type Key.comparator_witness = comparator_witness

  module Set :
    Set.S_binable
    with type Elt.t = t
    with type Elt.comparator_witness = comparator_witness
end

module type S_binable = sig
  include S_common

  include
    Map_and_set_binable
    with type t := t
    with type comparator_witness := comparator_witness
end


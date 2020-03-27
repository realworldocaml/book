open! Import
open! Std_internal

module type S_kernel_without_zone = Time0_intf.S
module type S_kernel = Time_intf.S

include Time_intf.S with module Time := Time_float0

module Stable : sig
  include module type of struct
    include Time_float0.Stable
  end

  module With_utc_sexp : sig
    module V2 : sig
      type t = Time_float0.t [@@deriving hash]

      include Stable_without_comparator with type t := t
    end
  end

  module Zone : Zone_intf.S_stable with type t := Zone.t
end

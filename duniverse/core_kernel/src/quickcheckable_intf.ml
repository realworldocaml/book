open! Import

module type Conv = sig
  type quickcheckable
  type t

  val of_quickcheckable : quickcheckable -> t
  val to_quickcheckable : t -> quickcheckable
end

module type Conv_filtered = sig
  type quickcheckable
  type t

  val of_quickcheckable : quickcheckable -> t option
  val to_quickcheckable : t -> quickcheckable
end

(** Provides functors for making a module quickcheckable with {!Quickcheck}. *)
module type Quickcheckable = sig
  module type Conv = Conv
  module type Conv_filtered = Conv_filtered
  module type S = Quickcheck.S
  module type S1 = Quickcheck.S1
  module type S2 = Quickcheck.S2
  module type S_int = Quickcheck.S_int

  module Of_quickcheckable
      (Quickcheckable : S)
      (Conv : Conv with type quickcheckable := Quickcheckable.t) :
    S with type t := Conv.t

  module Of_quickcheckable_filtered
      (Quickcheckable : S)
      (Conv : Conv_filtered with type quickcheckable := Quickcheckable.t) :
    S with type t := Conv.t
end

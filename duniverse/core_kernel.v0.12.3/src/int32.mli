(** This module extends {{!Base.Int32}[Base.Int32]}. *)

(** {2 Interface from Base} *)

(** @open *)
include
module type of struct
  include Base.Int32
end
  with module Hex := Base.Int32.Hex

(** {2 Extensions} *)

(** @open *)
include
  Int_intf.Extension with type t := t and type comparator_witness := comparator_witness

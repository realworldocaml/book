(** This module extends {{!Base.Int64}[Base.Int64]}. *)

(** {2 Interface from Base} *)

(** @open *)
include module type of struct
  include Base.Int64
end

(** {2 Extensions} *)

(** @open *)
include
  Int_intf.Extension with type t := t and type comparator_witness := comparator_witness

(** Just a wrapper around
    [Base.Not_exposed_properly.Import.Int_replace_polymorphic_compare] *)

open! Import

include Base.Not_exposed_properly.Import.Int_replace_polymorphic_compare  (** @open *)

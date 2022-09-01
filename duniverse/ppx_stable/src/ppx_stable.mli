open Ppxlib

(** Construct an attribute containing the list of stable changes to include in the stable
    converters. If attached to a type definition, this attributes obsoletes and prevents
    the use of stable change arguments to the [@@deriving] attribute. *)
val make_stable_changes_attribute
  :  loc:Location.t
  -> ?add:string list
  -> ?modify:string list
  -> ?set:string list
  -> ?remove:string list
  -> unit
  -> attribute

val stable_record : Deriving.t
val stable_variant : Deriving.t

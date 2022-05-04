(**
   TypeScript-specific ATD annotations.

   This interface serves as a reference of which TypeScript-specific
   ATD annotations are supported. Atdts also honors JSON-related annotations
   defined in [Atdgen_emit.Json].
*)

(** Whether an association list of ATD type [(string * foo) list]
    must be represented in TypeScript as an array of pairs or as a map.
    This is independent of the JSON representation.
*)
type assoc_repr =
  | Array
  | Map

(** Extract ["42"] from [<ts default="42">].
    The provided default must be a well-formed TypeScript expression.
*)
val get_ts_default : Atd.Annot.t -> string option

(** Inspect annotations placed on arrays of pairs such as
    [(string * foo) list <ts repr="map">].
    Permissible values for the [repr] field are ["map"] and ["array"].
    The default is ["array"].
*)
val get_ts_assoc_repr : Atd.Annot.t -> assoc_repr

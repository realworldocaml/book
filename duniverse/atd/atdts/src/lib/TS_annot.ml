(*
   ATD annotations to be interpreted specifically by atdts.

   Atdts also honors json-related annotations defined in Atdgen_emit.Json.
*)

type assoc_repr =
  | Array
  | Map

let get_ts_default an : string option =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["ts"]
      ~field:"default"
      an

let get_ts_assoc_repr an : assoc_repr =
  Atd.Annot.get_field
    ~parse:(function
      | "array" -> Some Array
      | "map" -> Some Map
      | _ -> None
    )
    ~default:Array
    ~sections:["ts"]
    ~field:"repr"
    an

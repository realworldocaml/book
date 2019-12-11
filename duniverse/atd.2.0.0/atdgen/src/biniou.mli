(** Biniou-specific options derived from ATD annotations. *)

type biniou_int =
    [ `Svint | `Uvint | `Int8 | `Int16 | `Int32 | `Int64 ]

type biniou_float = [ `Float32 | `Float64 ]

type biniou_list = [ `Array | `Table ]

type biniou_field = { biniou_unwrapped : bool }

(** Biniou-specific options that decorate each kind of ATD AST node. *)
type biniou_repr =
  | Unit
  | Bool
  | Int of biniou_int
  | Float of biniou_float

  | String
  | Sum
  | Record
  | Tuple
  | List of biniou_list
  | Option
  | Nullable
  | Wrap
  | External

  | Cell
  | Field of biniou_field
  | Variant
  | Def

val get_biniou_float : Atd.Annot.t -> biniou_float
val get_biniou_int : Atd.Annot.t -> biniou_int
val get_biniou_list : Atd.Annot.t -> biniou_list

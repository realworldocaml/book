type t =
  { x : int
  ; mutable y : bool
  }
[@@deriving_inline fields ~fold_right]

include sig
  [@@@ocaml.warning "-32-60"]

  val y : t -> bool
  val set_y : t -> bool -> unit
  val x : t -> int

  module Fields : sig
  val names : string list
  val y : (t, bool) Fieldslib.Field.t
  val x : (t, int) Fieldslib.Field.t

  val fold
    :  init:'acc__0
    -> x:('acc__0 -> (t, int) Fieldslib.Field.t -> 'acc__1)
    -> y:('acc__1 -> (t, bool) Fieldslib.Field.t -> 'acc__2)
    -> 'acc__2

  val fold_right
    :  x:((t, int) Fieldslib.Field.t -> 'acc__1 -> 'acc__2)
    -> y:((t, bool) Fieldslib.Field.t -> 'acc__0 -> 'acc__1)
    -> init:'acc__0
    -> 'acc__2

  val make_creator
    :  x:((t, int) Fieldslib.Field.t -> 'acc__0 -> ('input__ -> int) * 'acc__1)
    -> y:((t, bool) Fieldslib.Field.t -> 'acc__1 -> ('input__ -> bool) * 'acc__2)
    -> 'acc__0
    -> ('input__ -> t) * 'acc__2

  val create : x:int -> y:bool -> t

  val map
    :  x:((t, int) Fieldslib.Field.t -> int)
    -> y:((t, bool) Fieldslib.Field.t -> bool)
    -> t

  val iter
    :  x:((t, int) Fieldslib.Field.t -> unit)
    -> y:((t, bool) Fieldslib.Field.t -> unit)
    -> unit

  val for_all
    :  x:((t, int) Fieldslib.Field.t -> bool)
    -> y:((t, bool) Fieldslib.Field.t -> bool)
    -> bool

  val exists
    :  x:((t, int) Fieldslib.Field.t -> bool)
    -> y:((t, bool) Fieldslib.Field.t -> bool)
    -> bool

  val to_list
    :  x:((t, int) Fieldslib.Field.t -> 'elem__)
    -> y:((t, bool) Fieldslib.Field.t -> 'elem__)
    -> 'elem__ list

  val map_poly : ([< `Read | `Set_and_create ], t, 'x0) Fieldslib.Field.user -> 'x0 list

  module Direct : sig
  val iter
    :  t
    -> x:((t, int) Fieldslib.Field.t -> t -> int -> unit)
    -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> unit)
    -> unit

  val fold
    :  t
    -> init:'acc__0
    -> x:('acc__0 -> (t, int) Fieldslib.Field.t -> t -> int -> 'acc__1)
    -> y:('acc__1 -> (t, bool) Fieldslib.Field.t -> t -> bool -> 'acc__2)
    -> 'acc__2

  val for_all
    :  t
    -> x:((t, int) Fieldslib.Field.t -> t -> int -> bool)
    -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> bool)
    -> bool

  val exists
    :  t
    -> x:((t, int) Fieldslib.Field.t -> t -> int -> bool)
    -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> bool)
    -> bool

  val to_list
    :  t
    -> x:((t, int) Fieldslib.Field.t -> t -> int -> 'elem__)
    -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> 'elem__)
    -> 'elem__ list

  val fold_right
    :  t
    -> x:((t, int) Fieldslib.Field.t -> t -> int -> 'acc__1 -> 'acc__2)
    -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> 'acc__0 -> 'acc__1)
    -> init:'acc__0
    -> 'acc__2

  val map
    :  t
    -> x:((t, int) Fieldslib.Field.t -> t -> int -> int)
    -> y:((t, bool) Fieldslib.Field.t -> t -> bool -> bool)
    -> t

  val set_all_mutable_fields : t -> y:bool -> unit
end
end
end
[@@ocaml.doc "@inline"]

[@@@end]

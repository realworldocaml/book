(** First-class representative of an individual variant in a variant type, used in
    [[@@deriving variants]]. *)

type 'constructor t =
  { name : string
  (** The position of the constructor in the type definition, starting from 0 *)
  ; rank : int
  ; constructor : 'constructor
  }

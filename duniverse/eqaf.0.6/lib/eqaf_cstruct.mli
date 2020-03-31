val equal : Cstruct.t -> Cstruct.t -> bool
val compare_be : Cstruct.t -> Cstruct.t -> int
val compare_be_with_len : len:int -> Cstruct.t -> Cstruct.t -> int
val compare_le : Cstruct.t -> Cstruct.t -> int
val compare_le_with_len : len:int -> Cstruct.t -> Cstruct.t -> int

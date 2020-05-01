val equal : Cstruct.t -> Cstruct.t -> bool
val compare_be : Cstruct.t -> Cstruct.t -> int
val compare_be_with_len : len:int -> Cstruct.t -> Cstruct.t -> int
val compare_le : Cstruct.t -> Cstruct.t -> int
val compare_le_with_len : len:int -> Cstruct.t -> Cstruct.t -> int
val find_uint8 : ?off:int -> f:(int -> bool) -> Cstruct.t -> int
val exists_uint8 : ?off:int -> f:(int -> bool) -> Cstruct.t -> bool

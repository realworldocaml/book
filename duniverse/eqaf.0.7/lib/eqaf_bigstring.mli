type bigstring = (char, Bigarray_compat.int8_unsigned_elt, Bigarray_compat.c_layout) Bigarray_compat.Array1.t

val equal : bigstring -> bigstring -> bool
val compare_be : bigstring -> bigstring -> int
val compare_be_with_len : len:int -> bigstring -> bigstring -> int
val compare_le : bigstring -> bigstring -> int
val compare_le_with_len : len:int -> bigstring -> bigstring -> int
val find_uint8 : ?off:int -> f:(int -> bool) -> bigstring -> int
val exists_uint8 : ?off:int -> f:(int -> bool) -> bigstring -> bool

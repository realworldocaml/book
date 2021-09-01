module type Getters = sig
  val get : Bigstringaf.t -> int -> char

  val get_int16_le : Bigstringaf.t -> int -> int
  val get_int16_sign_extended_le : Bigstringaf.t -> int -> int
  val get_int32_le : Bigstringaf.t -> int -> int32
  val get_int64_le : Bigstringaf.t -> int -> int64

  val get_int16_be : Bigstringaf.t -> int -> int
  val get_int16_sign_extended_be : Bigstringaf.t -> int -> int
  val get_int32_be : Bigstringaf.t -> int -> int32
  val get_int64_be : Bigstringaf.t -> int -> int64
end

module type Setters = sig
  val set : Bigstringaf.t -> int -> char -> unit

  val set_int16_le : Bigstringaf.t -> int -> int -> unit
  val set_int32_le : Bigstringaf.t -> int -> int32 -> unit
  val set_int64_le : Bigstringaf.t -> int -> int64 -> unit

  val set_int16_be : Bigstringaf.t -> int -> int -> unit
  val set_int32_be : Bigstringaf.t -> int -> int32 -> unit
  val set_int64_be : Bigstringaf.t -> int -> int64 -> unit
end

module type Blit = sig
  val blit : Bigstringaf.t -> src_off:int -> Bigstringaf.t -> dst_off:int -> len:int -> unit
  val blit_from_string : String.t -> src_off:int -> Bigstringaf.t -> dst_off:int -> len:int -> unit
  val blit_from_bytes  : Bytes.t  -> src_off:int -> Bigstringaf.t -> dst_off:int -> len:int -> unit

  val blit_to_bytes : Bigstringaf.t -> src_off:int -> Bytes.t -> dst_off:int -> len:int -> unit
end

module type Memcmp = sig
  val memcmp : Bigstringaf.t -> int -> Bigstringaf.t -> int -> int -> int
  val memcmp_string : Bigstringaf.t -> int -> String.t -> int -> int -> int
end

module type Memchr = sig
  val memchr : Bigstringaf.t -> int -> char -> int -> int
end

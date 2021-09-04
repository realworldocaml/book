external set_int32_ne : bytes -> int -> int32 -> unit = "%caml_string_set32"
external get_int64_ne : bytes -> int -> int64 = "%caml_string_get64"

external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

let set_int32_le b i x =
  if Sys.big_endian then set_int32_ne b i (swap32 x)
  else set_int32_ne b i x

let get_int64_le b i =
  if Sys.big_endian then swap64 (get_int64_ne b i)
  else get_int64_ne b i

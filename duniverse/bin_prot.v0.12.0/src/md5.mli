type t = Md5_lib.t

include Binable.Minimal.S with type t := t

val to_hex : t -> string
val of_hex_exn : string -> t

val compare : t -> t -> int

val to_binary : t -> string
val of_binary_exn : string -> t
val unsafe_of_binary : string -> t

type t
val empty: t
val touch: t -> string -> t
val to_list: t -> (string*int) list

[@@@part "1"]
val count : t -> string -> int

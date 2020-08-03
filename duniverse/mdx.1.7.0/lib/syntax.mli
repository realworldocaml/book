type t = Normal | Cram | Mli

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val infer : file:string -> t option

val of_string : string -> t option

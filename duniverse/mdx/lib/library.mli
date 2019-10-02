type t =
  { base_name : string
  ; sub_lib : string option
  }

val equal : t -> t -> bool

val compare : t -> t -> int

val pp : t Fmt.t

val from_string : string -> (t, string) Result.result

module Set : sig
  include Set.S with type elt = t

  val to_package_set : t -> Astring.String.Set.t
end

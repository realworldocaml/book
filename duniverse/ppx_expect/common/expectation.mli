module Body : sig
  type 'a t =
    | Exact of string
    | Output
    | Pretty of 'a
    | Unreachable
  [@@deriving sexp_of, compare]

  val map_pretty : 'a t -> f:('a -> 'b) -> 'b t
end

type 'a t =
  { tag : string option (** Tag of the string payload *)
  ; body : 'a Body.t
  ; extid_location : File.Location.t
  (** Location of the extension id ("expect" or
      "expect_exact") *)
  ; body_location : File.Location.t
  (** Location of the string payload of the extension
      point *)
  }
[@@deriving sexp_of, compare]

module Raw : sig
  type nonrec t = string t [@@deriving sexp_of, compare]
end

val map_pretty : 'a t -> f:('a -> 'b) -> 'b t

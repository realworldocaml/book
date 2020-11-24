(** Conversions between JSON and biniou *)

val biniou_of_json : Yojson.Safe.t -> Bi_io.tree
  (** Converts from JSON to biniou.
      @raise Failure if conversion is not reversible. *)

val json_of_biniou : Bi_io.tree -> Yojson.Safe.t
  (** Converts from biniou to JSON.
      @raise Failure if conversion is not reversible, except for biniou
      tables which are considered equivalent to arrays of records. *)

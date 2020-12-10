(** Curve parameters.
    They are stored as [Hex.t] because [Fe.t] and [Cstruct.t] are
    mutable. *)

val a : Hex.t

val b : Hex.t

val g_x : Hex.t
(** The base point's x coordinate. *)

val g_y : Hex.t
(** The base point's y coordinate. *)

val p : Hex.t
(** The prime number corresponding to [Fe]. *)

val n : Hex.t
(** The group order. *)

type t
(** A Point on the P-256 curve. *)

val at_infinity : unit -> t
(** The point at infinity *)

val is_infinity : t -> bool
(** [is_infty p] checks wether [p] is the point at infinity. *)

val add : t -> t -> t
(** Point addition. [add p q] returns the result of the addition of [p] and [q]. *)

val double : t -> t
(** Point doubling. [double p] returns the result of doubling [p]. *)

val of_cstruct :
  Cstruct.t ->
  ( t,
    [> `Invalid_format | `Invalid_length | `Invalid_range | `Not_on_curve ] )
  result
(** Convert from cstruct. The format is the uncompressed format described in
    SEC1, section 2.3.4, that is to say:

    - the point at infinity is the single byte "00".
    - for a point (x, y) not at infinity, the format is:
      - the byte "04"
      - x serialized in big endian format, padded to 32 bytes.
      - y serialized in big endian format, padded to 32 bytes.

    @see <http://www.secg.org/sec1-v2.pdf>
*)

val to_cstruct : t -> Cstruct.t
(** Convert to a cstruct. See [of_cstruct] for the format. *)

val x_of_finite_point : t -> Cstruct.t
(** Return only the X coordinate of a point that is not at infinity. *)

val params_g : t
(** The curve's base point *)

val select : bool -> then_:t -> else_:t -> t
(** Constant-time selection. See [Field_element.select]. *)

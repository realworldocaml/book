
(** {{:https://tools.ietf.org/html/rfc5869}RFC 5869} specifies a HMAC-based
    Extract-and-Expand Key Derivation Function (HKDF), which is abstracted over
    a specific hash function. *)

module type S = sig

  (** [extract salt ikm] is [prk], the pseudorandom key of hash length octets.
      The [salt] is an optional non-secret random value, [ikm] the input key
      material. *)
  val extract : ?salt:Cstruct.t -> Cstruct.t -> Cstruct.t

  (** [extract prk info length] is [okm], the output keying material.  Given the
  pseudorandom key of hash length (usually output of [!extract] step), and an
  optional context and application specific information [info], the [okm] is
  generated. *)
  val expand : prk:Cstruct.t -> ?info:Cstruct.t -> int -> Cstruct.t
end

(** Given a Hash function, get the HKDF *)
module Make (H : Mirage_crypto.Hash.S) : S

(** convenience [extract hash salt ikm] where the [hash] has to be provided explicitly *)
val extract : hash:Mirage_crypto.Hash.hash -> ?salt:Cstruct.t -> Cstruct.t -> Cstruct.t

(** convenience [expand hash prk info len] where the [hash] has to be provided explicitly *)
val expand : hash:Mirage_crypto.Hash.hash -> prk:Cstruct.t -> ?info:Cstruct.t -> int -> Cstruct.t

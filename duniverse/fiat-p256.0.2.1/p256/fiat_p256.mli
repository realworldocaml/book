(** See {!page-implementation} for a description on how this is implemented. *)

type error =
  [ `Invalid_range
  | `Invalid_format
  | `Invalid_length
  | `Not_on_curve
  | `At_infinity ]
(** The type for public key parsing errors. *)

val pp_error : Format.formatter -> error -> unit
(** Pretty printer for public key parsing errors *)

type secret
(** Type for P256 private keys.

    In the usual setting, the private key only be generated and used for key
    exchange. But it can be useful to create values of type [secret] with a
    known value, for example to check against test vectors.
    One can use the following pattern to do this:

    {[ let (secret, _) = gen_key ~rng:(fun _ -> known_data) ]}
*)

val gen_key : rng:(int -> Cstruct.t) -> secret * Cstruct.t
(** [gen_key ~rng] generates a private and a public key for Ephemeral Diffie-Hellman
    over P256. The returned key pair MUST only be used for a single key exchange.

    [rng] is the function used to repeteadly generate a private key until a valid candidate
    is obtained. [rng]'s int parameter is the size of the [Cstruct.t] to generate.
    If [rng] returns an invalid length buffer, [Failure _] is raised.

    The generated private key is checked to be greater than zero and lower than the group
    order meaning the public key cannot be the point at inifinity. *)

val key_exchange : secret -> Cstruct.t -> (Cstruct.t, error) result
(** [key_exchange secret received_public_key] performs Diffie-Hellman key exchange
    using your secret and the data received from the other party. Returns the shared secret
    or an error if the received data is wrongly encoded, doesn't represent a point on the curve
    or represent the point at infinity.

    The shared secret is returned as is i.e. not stripped from leading 0x00 bytes.

    @see <http://www.secg.org/sec1-v2.pdf> for public key encoding format. *)

(** {{:https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange}
    Diffie-Hellman key exchange} over
    {{:https://en.wikipedia.org/wiki/Curve25519} Curve25519} (also known as
    X25519).

    This implementation uses C code from {{:https://project-everest.github.io/}
    Project Everest}, an effort to build and deploy a verified HTTPS stack.

    @see <https://tools.ietf.org/html/rfc7748> RFC7748, "Elliptic Curves for
    Security" - where this algorithm is defined.

    @see <https://tools.ietf.org/html/rfc8446#section-7.4.2> RFC8446, "The
    Transport Layer Security (TLS) Protocol Version 1.3", section 7.4.2 - how to
    use this in the context of TLS 1.3.
*)

type secret
(** Key material. In elliptic curve terms, a scalar.

    To generate a key pair, use [gen_key].

    In the usual setting, the private key only be generated and used for key
    exchange. But it can be useful to create values of type [secret] with a
    known value, for example to check against test vectors.
    One can use the following pattern to do this:

    {[ let (secret, _) = gen_key ~rng:(fun _ -> known_data) ]}
*)

val gen_key : rng:(int -> Cstruct.t) -> secret * Cstruct.t
(** Generate a key pair. [rng] should return a [Cstruct.t] with the specified
    key length (in bytes) and fill it with random bytes.

    If the cstruct returned by [rng] does not have the correct length, raises
    [Failure _]. *)

type error = [ `Invalid_length | `Low_order ]
(** Kind of errors. *)

val pp_error : Format.formatter -> error -> unit
(** Pretty printer for errors *)

val key_exchange : secret -> Cstruct.t -> (Cstruct.t, error) result
(** Perform Diffie-Hellman key exchange between a private part and a public
    part.

    It checks length of the [pub] key and returns an error if it has an
    incorrect length.

    In DH terms, the private part corresponds to a scalar, and the public part
    corresponds to a point, and this computes the scalar multiplication.

    The resulting shared secret is not truncated.

    As described in {{: https://tools.ietf.org/html/rfc7748#section-6.1} RFC
    7748, section 6.1}, this function might internally generate an all-zero
    value. If this is the case [Error `Low_order] will be returned instead. {{:
    https://tools.ietf.org/html/rfc8446#section-7.4.2} This check is necessary
    in the context of TLS 1.3}, but might not in other protocols. *)

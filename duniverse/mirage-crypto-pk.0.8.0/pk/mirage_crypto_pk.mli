(** {1 Public-key cryptography} *)

(** Public and private key types are private, the constructors validate their
    well-formedness as much as possible, esp. so that [powm_sec] will not raise
    an exception (exponent > 1, or odd modulus). All modular exponentiations
    (unless otherwise noted) use the {!Z.powm_sec} function, which uses a static
    access pattern and operates in constant time (of the bit size of the input),
    independent of which bits are set and not set. The performance is up to 20%
    worse than [powm]. Additionally, blinding is applied to RSA and DSA by
    default. *)

type bits = int

(** {b RSA} public-key cryptography algorithm. *)
module Rsa : sig

  (** {1 Keys}

      Messages are checked not to exceed the key size, and this is signalled via
      the {!Insufficient_key} exception.

      Private-key operations are optionally protected through RSA blinding. *)

  exception Insufficient_key
  (** Raised if the key is too small to transform the given message, i.e. if the
      numerical interpretation of the (potentially padded) message is not
      smaller than the modulus. *)

  type pub = private {
    e : Z.t ; (** Public exponent *)
    n : Z.t ; (** Modulus *)
  }
  (** The public portion of the key.

      {e [Sexplib] convertible}. *)

  val pub : e:Z.t -> n:Z.t -> (pub, [> `Msg of string ]) result
  (** [pub ~e ~n] validates the public key: [1 < e < n], [n > 0],
      [is_odd n], and [numbits n >= 89] (a requirement for PKCS1 operations). *)

  type priv = private {
    e  : Z.t ; (** Public exponent *)
    d  : Z.t ; (** Private exponent *)
    n  : Z.t ; (** Modulus ([p q])*)
    p  : Z.t ; (** Prime factor [p] *)
    q  : Z.t ; (** Prime factor [q] *)
    dp : Z.t ; (** [d mod (p-1)] *)
    dq : Z.t ; (** [d mod (q-1)] *)
    q' : Z.t ; (** [q^(-1) mod p] *)
  }
  (** Full private key (two-factor version).

      {b Note} The key layout assumes that [p > q], which affects the quantity
      [q'] (sometimes called [u]), and the computation of the private transform.
      Some systems assume otherwise. When using keys produced by a system that
      computes [u = p^(-1) mod q], either exchange [p] with [q] and [dp] with
      [dq], or re-generate the full private key using
      {{!priv_of_primes}[priv_of_primes]}.

      {e [Sexplib] convertible}. *)

  val priv : e:Z.t -> d:Z.t -> n:Z.t -> p:Z.t -> q:Z.t -> dp:Z.t -> dq:Z.t ->
    q':Z.t -> (priv, [> `Msg of string ]) result
  (** [priv ~e ~d ~n ~p ~q ~dp ~dq ~q'] validates the private key: [e, n] must
      be a valid {!pub}, [p] and [q] valid prime numbers [> 0], [odd],
      probabilistically prime, [p <> q], [n = p * q], [e] probabilistically
      prime and coprime to both [p] and [q], [q' = q ^ -1 mod p], [1 < d < n],
      [dp = d mod (p - 1)], [dq = d mod (q - 1)],
      and [d = e ^ -1 mod (p - 1) (q - 1)]. *)

  val pub_bits : pub -> bits
  (** Bit-size of a public key. *)

  val priv_bits : priv -> bits
  (** Bit-size of a private key. *)

  val priv_of_primes : e:Z.t -> p:Z.t -> q:Z.t ->
    (priv, [> `Msg of string ]) result
  (** [priv_of_primes ~e ~p ~q] is the {{!priv}private key} derived from the
      minimal description [(e, p, q)]. *)

  val priv_of_exp : ?g:Mirage_crypto_rng.g -> ?attempts:int -> e:Z.t -> d:Z.t ->
    n:Z.t -> unit -> (priv, [> `Msg of string ]) result
  (** [priv_of_exp ?g ?attempts ~e ~d n] is the unique {{!priv}private key}
      characterized by the public ([e]) and private ([d]) exponents, and modulus
      [n]. This operation uses a probabilistic process that can fail to recover
      the key.

      [~attempts] is the number of trials. For triplets that form an RSA key,
      the probability of failure is at most [2^(-attempts)]. [attempts] defaults
      to an unspecified number that yields a very high probability of recovering
      valid keys.

      Note that no time masking is done for the computations in this function. *)

  val pub_of_priv : priv -> pub
  (** Extract the public component from a private key. *)

  (** {1 The RSA transformation} *)

  type 'a or_digest = [ `Message of 'a | `Digest of Mirage_crypto.Hash.digest ]
  (** Either an ['a] or its digest, according to some hash algorithm. *)

  type mask = [ `No | `Yes | `Yes_with of Mirage_crypto_rng.g ]
  (** Masking (cryptographic blinding) mode for the RSA transform with the
      private key. Masking does not change the result, but it does change the
      timing profile of the operation.

      {ul
      {- [`No] disables masking. It is slightly faster but it {b exposes the
         private key to timing-based attacks}.}
      {- [`Yes] uses random masking with the global RNG instance. This is
         the sane option.}
      {- [`Yes_with g] uses random masking with the generator [g].}} *)

  val encrypt : key:pub  -> Cstruct.t -> Cstruct.t
  (** [encrypt key message] is the encrypted [message].

      @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key})

      @raise Invalid_argument if [message] is [0x00] or [0x01]. *)

  val decrypt : ?crt_hardening:bool -> ?mask:mask -> key:priv ->
    Cstruct.t -> Cstruct.t
  (** [decrypt ~crt_hardening ~mask key ciphertext] is the decrypted
      [ciphertext], left-padded with [0x00] up to [key] size.

      [~crt_hardening] defaults to [false]. If [true] verifies that the
      result is correct. This is to counter Chinese remainder theorem attacks to
      factorize primes. If the computed signature is incorrect, it is again
      computed in the classical way (c ^ d mod n) without the Chinese remainder
      theorem optimization. The deterministic {{!PKCS1.sign}PKCS1 signing},
      which is at danger, uses [true] as default.

      [~mask] defaults to [`Yes].

      @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key}) *)

  (** {1 Key generation} *)

  val generate : ?g:Mirage_crypto_rng.g -> ?e:Z.t -> bits:bits -> unit -> priv
  (** [generate ~g ~e ~bits ()] is a new {{!priv}private key}. The new key is
      guaranteed to be well formed, see {!priv}.

      [e] defaults to [2^16+1].

      {b Note} This process might diverge if there are no keys for the given
      bit size. This can happen when [bits] is extremely small.

      @raise Invalid_argument if [e] is not a prime number (checked
      probabilistically) or not in the range [1 < e < 2^bits], or if
      [bits < 89] (as above, required for PKCS1 operations).  *)

  (** {1 PKCS#1 padded modes} *)

  (** {b PKCS v1.5} operations, as defined by {b PKCS #1 v1.5}.

      For the operations that only add the raw padding, the key size must be at
      least 11 bytes larger than the message. For full {{!sign}signing}, the
      minimal key size varies according to the hash algorithm. In this case, the
      key size is [priv_bits key / 8], rounded up. *)
  module PKCS1 : sig

    val encrypt : ?g:Mirage_crypto_rng.g -> key:pub -> Cstruct.t -> Cstruct.t
    (** [encrypt g key message] is a PKCS1-padded (type 2) and encrypted
        [message].

        @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key}) *)

    val decrypt : ?crt_hardening:bool -> ?mask:mask -> key:priv ->
      Cstruct.t -> Cstruct.t option
    (** [decrypt ~crt_hardening ~mask ~key ciphertext] is [Some message] if
        the [ciphertext] was produced by the corresponding {{!encrypt}encrypt}
        operation, or [None] otherwise. [crt_hardening] defaults to
        [false]. *)

    val sig_encode : ?crt_hardening:bool -> ?mask:mask -> key:priv ->
      Cstruct.t -> Cstruct.t
    (** [sig_encode ~crt_hardening ~mask ~key message] is the PKCS1-padded
        (type 1) [message] signed by the [key]. [crt_hardening] defaults to
        [true] and verifies that the computed signature is correct.

        {b Note} This operation performs only the padding and RSA transformation
        steps of the PKCS 1.5 signature. The full signature is implemented by
        {{!sign}[sign]}.

        @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key}) *)

    val sig_decode : key:pub -> Cstruct.t -> Cstruct.t option
    (** [sig_decode key signature] is [Some message] when the [signature]
        was produced with the given [key] as per {{!sig_encode}sig_encode}, or
        [None] *)

    val min_key : Mirage_crypto.Hash.hash -> bits
    (** [min_key hash] is the minimum key size required by {{!sign}[sign]}. *)

    val sign : ?crt_hardening:bool -> ?mask:mask ->
      hash:Mirage_crypto.Hash.hash -> key:priv -> Cstruct.t or_digest ->
      Cstruct.t
    (** [sign ~crt_hardening ~mask ~hash ~key message] is the PKCS 1.5
        signature of [message], signed by the [key], using the hash function
        [hash]. This is the full signature, with the ASN-encoded message digest
        as the payload. [crt_hardening] defaults to [true] and verifies that
        the computed signature is correct.

        [message] is either the actual message, or its digest.

        @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key})

        @raise Invalid_argument if message is a [`Digest] of the wrong size.  *)

    val verify : hashp:(Mirage_crypto.Hash.hash -> bool) -> key:pub ->
      signature:Cstruct.t -> Cstruct.t or_digest -> bool
    (** [verify ~hashp ~key ~signature message] checks that [signature] is the
        PKCS 1.5 signature of the [message] under the given [key].

        [message] is either the actual message, or its digest.

        [hashp] determines the allowed hash algorithms. Whenever [hashp] is
        [false], [verify] is also [false].

        @raise Invalid_argument if message is a [`Digest] of the wrong size.  *)
  end

  (** {1 OAEP padded modes} *)

  (** {b OAEP}-padded encryption, as defined by {b PKCS #1 v2.1}.

      The same hash function is used for padding and MGF. MGF is {b MGF1} as
      defined in {b PKCS #1 2.1}.

      Keys must have a minimum of [2 + 2 * hlen + len(message)] bytes, where
      [hlen] is the hash length. *)
  module OAEP (H : Mirage_crypto.Hash.S) : sig

    val encrypt : ?g:Mirage_crypto_rng.g -> ?label:Cstruct.t -> key:pub ->
      Cstruct.t -> Cstruct.t
    (** [encrypt ~g ~label ~key message] is {b OAEP}-padded and encrypted
        [message], using the optional [label].

        @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key}) *)

    val decrypt : ?crt_hardening:bool -> ?mask:mask -> ?label:Cstruct.t ->
      key:priv -> Cstruct.t -> Cstruct.t option
    (** [decrypt ~crt_hardening ~mask ~label ~key ciphertext] is
        [Some message] if the [ciphertext] was produced by the corresponding
        {{!encrypt}encrypt} operation, or [None] otherwise. [crt_hardening]
        defaults to [false]. *)
  end

  (** {1 PSS signing} *)

  (** {b PSS}-based signing, as defined by {b PKCS #1 v2.1}.

      The same hash function is used for padding, MGF and computing message
      digest. MGF is {b MGF1} as defined in {b PKCS #1 2.1}.

      Keys must have a minimum of [2 + hlen + slen] bytes, where [hlen] is the
      hash length and [slen] is the seed length. *)
  module PSS (H: Mirage_crypto.Hash.S) : sig

    val sign : ?g:Mirage_crypto_rng.g -> ?crt_hardening:bool ->
      ?mask:mask -> ?slen:int -> key:priv -> Cstruct.t or_digest -> Cstruct.t
    (** [sign ~g ~crt_hardening ~mask ~slen ~key message] the {!p PSS}-padded
        digest of [message], signed with the [key]. [crt_hardening] defaults
        to [false].

        [slen] is the optional seed length and defaults to the size of the
        underlying hash function.

        [message] is either the actual message, or its digest.

        @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key})

        @raise Invalid_argument if message is a [`Digest] of the wrong size.  *)

    val verify : ?slen:int -> key:pub -> signature:Cstruct.t -> Cstruct.t or_digest -> bool
    (** [verify ~slen ~key ~signature message] checks whether [signature] is a
        valid {b PSS} signature of the [message] under the given [key].

        [message] is either the actual message, or its digest.

        @raise Invalid_argument if message is a [`Digest] of the wrong size. *)
  end

  (**/**)
  val pub_of_sexp : Sexplib.Sexp.t -> pub
  val sexp_of_pub : pub -> Sexplib.Sexp.t

  val priv_of_sexp : Sexplib.Sexp.t -> priv
  val sexp_of_priv : priv -> Sexplib.Sexp.t
  (**/**)

end


(** {b DSA} digital signature algorithm. *)
module Dsa : sig

  (** {1 DSA signature algorithm} *)

  type priv = private {
    p  : Z.t ; (** Modulus *)
    q  : Z.t ; (** Subgroup order *)
    gg : Z.t ; (** Group Generator *)
    x  : Z.t ; (** Private key proper *)
    y  : Z.t ; (** Public component *)
  }
  (** Private key. [p], [q] and [gg] comprise {i domain parameters}.

      {e [Sexplib] convertible}. *)

  val priv : ?fips:bool -> p:Z.t -> q:Z.t -> gg:Z.t -> x:Z.t -> y:Z.t -> unit ->
    (priv, [> `Msg of string ]) result
  (** [priv ~fips ~p ~q ~gg ~x ~y ()] constructs a private DSA key from the given
      numbers. Will result in an error if parameters are ill-formed: same as
      {!pub}, and additionally [0 < x < q] and [y = g ^ x mod p]. Note that no
      time masking is done on the modular exponentiation. *)

  type pub = private {
    p  : Z.t ;
    q  : Z.t ;
    gg : Z.t ;
    y  : Z.t ;
  }
  (** Public key, a subset of {{!priv}private key}.

      {e [Sexplib] convertible}. *)

  val pub : ?fips:bool -> p:Z.t -> q:Z.t -> gg:Z.t -> y:Z.t -> unit ->
    (pub, [> `Msg of string ]) result
  (** [pub ~fips ~p ~q ~gg ~y ()] constructs a public DSA key from the given
      numbers. Will result in an error if the parameters are not well-formed:
      [one < gg < p], [q] probabilistically a prime, [p] probabilistically
      prime and odd, [0 < y < p], [q < p], and [p - 1 mod q = 0]. If [fips] is
      specified and [true] (defaults to [false]), only FIPS-specified bit length
      for [p] and [q] are accepted. *)

  type keysize = [ `Fips1024 | `Fips2048 | `Fips3072 | `Exactly of bits * bits ]
  (** Key size request. Three {e Fips} variants refer to FIPS-standardized
      L-values ([p] size) and imply the corresponding N ([q] size); The last
      variants specifies L and N directly. *)

  type mask = [ `No | `Yes | `Yes_with of Mirage_crypto_rng.g ]
  (** Masking (cryptographic blinding) option. *)

  val pub_of_priv : priv -> pub
  (** Extract the public component from a private key. *)

  val generate : ?g:Mirage_crypto_rng.g -> keysize -> priv
  (** [generate g size] is a fresh {{!priv}private} key. The domain parameters
      are derived using a modified FIPS.186-4 probabilistic process, but the
      derivation can not be validated. Note that no time masking is done for the
      modular exponentiations.

      {b Note} The process might diverge if it is impossible to find parameters
      with the given bit sizes. This happens when [n] gets too big for [l], if
      the [size] was given as [`Exactly (l, n)].

      @raise Invalid_argument if [size] is (`Exactly (l, n)), and either [l] or
      [n] is ridiculously small. *)

  val sign : ?mask:mask -> ?k:Z.t -> key:priv -> Cstruct.t -> Cstruct.t * Cstruct.t
  (** [sign ~mask ~k ~key digest] is the signature, a pair of {!Cstruct.t}s
      representing [r] and [s] in big-endian.

      [digest] is the full digest of the actual message.

      [k], the random component, can either be provided, or is deterministically
      derived as per RFC6979, using SHA256.

      @raise Invalid_argument if [k] is unsuitable (leading to r or s being 0).
 *)

  val verify : key:pub -> Cstruct.t * Cstruct.t -> Cstruct.t -> bool
  (** [verify ~key (r, s) digest] verifies that the pair [(r, s)] is the signature
      of [digest], the message digest, under the private counterpart to [key]. *)

  val massage : key:pub -> Cstruct.t -> Cstruct.t
  (** [massage key digest] is the numeric value of [digest] taken modulo [q] and
      represented in the leftmost [bits(q)] bits of the result.

      Both FIPS.186-4 and RFC6979 specify that only the leftmost [bits(q)] bits of
      [digest] are to be taken into account, but some implementations consider the
      entire [digest]. In cases where {{!sign}sign} and {{!verify}verify} seem
      incompatible with a given implementation (esp. if {{!sign}sign} produces
      signatures with the [s] component different from the other
      implementation's), it might help to pre-process [digest] using this
      function (e.g. [sign ~key (massage ~key:(pub_of_priv key) digest)]).  *)

  (** [K_gen] can be instantiated over a hashing module to obtain an RFC6979
      compliant [k]-generator for that hash. *)
  module K_gen (H : Mirage_crypto.Hash.S) : sig

    val generate : key:priv -> Cstruct.t -> Z.t
    (** [generate key digest] deterministically takes the given private key and
        message digest to a [k] suitable for seeding the signing process. *)
  end

  (**/**)
  val pub_of_sexp : Sexplib.Sexp.t -> pub
  val sexp_of_pub : pub -> Sexplib.Sexp.t

  val priv_of_sexp : Sexplib.Sexp.t -> priv
  val sexp_of_priv : priv -> Sexplib.Sexp.t
  (**/**)

end


(** Diffie-Hellman, MODP version. *)
module Dh : sig

  (** {1 Diffie-Hellman key exchange} *)

  exception Invalid_key
  (** Raised if the private key material is degenerate.
      The following invariants are checked:
      Secret key: [1 < secret < p]
      Public key: [1 < public < p-1] && [public <> gg]
  *)

  type group = private {
    p  : Z.t ;        (** modulus *)
    gg : Z.t ;        (** generator *)
    q  : Z.t option ; (** subgroup order; potentially unknown *)
  }
  (** A DH group.

      {e [Sexplib] convertible}. *)

  val group : p:Z.t -> gg:Z.t -> ?q:Z.t -> unit ->
    (group, [> `Msg of string ]) result
  (** [group ~p ~gg ~q ()] constructs a group if [p] is odd, a prime number,
      and greater than [zero]. [gg] must be in the range [1 < gg < p]. *)

  type secret = private { group : group ; x : Z.t }
  (** A private key.

      {e [Sexplib] convertible.} *)

  val modulus_size : group -> bits
  (** Bit size of the modulus. *)

  val key_of_secret : group -> s:Cstruct.t -> secret * Cstruct.t
  (** [key_of_secret group s] is the {!secret} and the corresponding public
      key which use [s] as the secret exponent.

      @raise Invalid_key if [s] is degenerate. *)

  val gen_key : ?g:Mirage_crypto_rng.g -> ?bits:bits -> group -> secret * Cstruct.t
  (** Generate a random {!secret} and the corresponding public key.
      [bits] is the exact bit-size of {!secret} and defaults to a value
      dependent on the {!group}'s [p].

      {b Note} The process might diverge when [bits] is extremely small. *)

  val shared : secret -> Cstruct.t -> Cstruct.t option
  (** [shared secret public] is [Some shared_key] given a
      a previously generated {!secret} (which specifies the [group])
      and the other party's public key.
      [shared_key] is the unpadded big-endian representation of the shared key.
      It is [None] if these invariants do not hold for [public]:
      [1 < public < p-1] && [public <> gg]. *)

  val gen_group : ?g:Mirage_crypto_rng.g -> bits:bits -> unit -> group
  (** [gen_group ~g ~bits ()] generates a random {!group} with modulus size
      [bits]. Uses a safe prime [p = 2q + 1] (with [q] prime) for the modulus
      and [2] for the generator, such that [2^q = 1 mod p].
      Runtime is on the order of a minute for 1024 bits.
      Note that no time masking is done for the modular exponentiation.

      {b Note} The process might diverge if there are no suitable groups. This
      happens with extremely small [bits] values. *)

  (** A small catalog of standardized {!group}s. *)
  module Group : sig

    (** From RFC 2409: *)

    val oakley_1 : group
    val oakley_2 : group

    (** From RFC 3526: *)

    val oakley_5  : group
    val oakley_14 : group
    val oakley_15 : group
    val oakley_16 : group
    val oakley_17 : group
    val oakley_18 : group

    (** From RFC 5114: *)

    val rfc_5114_1 : group
    val rfc_5114_2 : group
    val rfc_5114_3 : group

    (** From draft-ietf-tls-negotiated-ff-dhe-08 *)

    val ffdhe2048 : group
    val ffdhe3072 : group
    val ffdhe4096 : group
    val ffdhe6144 : group
    val ffdhe8192 : group

  end

  (**/**)
  val group_of_sexp : Sexplib.Sexp.t -> group
  val sexp_of_group : group -> Sexplib.Sexp.t

  val secret_of_sexp : Sexplib.Sexp.t -> secret
  val sexp_of_secret : secret -> Sexplib.Sexp.t
  (**/**)

end

(** {b Z} Convert Z to big endian Cstruct.t and generate random Z values. *)
module Z_extra : sig
  (** {1 Conversion to and from Cstruct.t} *)

  val of_cstruct_be : ?bits:bits -> Cstruct.t -> Z.t
  (** [of_cstruct_be ~bits cs] interprets the bit pattern of [cs] as a
      {{!t}[t]} in big-endian.

      If [~bits] is not given, the operation considers the entire [cs],
      otherwise the initial [min ~bits (bit-length cs)] bits of [cs].

      Assuming [n] is the number of bits to extract, the [n]-bit in [cs] is
      always the least significant bit of the result. Therefore:
      {ul
      {- if the bit size [k] of [t] is larger than [n], [k - n] most
         significant bits in the result are [0]; and}
      {- if [k] is smaller than [n], the result contains [k] last of the [n]
         first bits of [cs].}} *)

  val to_cstruct_be : ?size:int -> Z.t -> Cstruct.t
  (** [to_cstruct_be ~size t] is the big-endian representation of [t].

      If [~size] is not given, it defaults to the minimal number of bytes
      needed to represent [t], which is [bits t / 8] rounded up.

      The least-significant bit of [t] is always the last bit in the result.
      If the size is larger than needed, the output is padded with zero bits.
      If it is smaller, the high bits in [t] are dropped. *)

  val into_cstruct_be : Z.t -> Cstruct.t -> unit
  (** [into_cstruct_be t cs] writes the big-endian representation of [t] into
      [cs]. It behaves like {{!to_cstruct_be}[to_cstruct_be]}, with [~size]
      spanning the entire [cs]. *)

  (** {1 Random generation} *)

  val gen : ?g:Mirage_crypto_rng.g -> Z.t -> Z.t
  (** [gen ~g n] picks a value in the interval [\[0, n - 1\]] uniformly at random. *)

  val gen_r : ?g:Mirage_crypto_rng.g -> Z.t -> Z.t -> Z.t
  (** [gen_r ~g low high] picks a value from the interval [\[low, high - 1\]]
      uniformly at random. *)
end

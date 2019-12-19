(** Simpler crypto

    Nocrypto is a cryptographic library.

    The overarching API principle is simply mapping inputs to outputs, wherever
    feasible.

    Similar algorithms in the same class (like {{!Hash}hashes} or
    {{!Cipher_block}block ciphers}) are presented as distinct modules sharing
    the same signature.

    {{!Rng}Randomness} is treated as an ambient effect.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(*
 * Doc note: Sexplib conversions are noted explicitly instead of using
 * `[@@deriving sexp]` because the syntax extension interacts badly with
 * ocamldoc.
 *)

(** {1 Utilities} *)

(** Base64 conversion.

    It is here only temporary, until we find it a proper home. *)
module Base64 : sig
  val encode : Cstruct.t -> Cstruct.t
  val decode : Cstruct.t -> Cstruct.t option
  val is_base64_char : char -> bool
end


(**/**)

(** A treasure-trove of random utilities.

    This is largely an internal API used in related sub-libraries or tests. As
    such, it is prone to breakage. *)
module Uncommon : sig

  val (//) : int -> int -> int
  (** [x // y] is the ceiling division [ceil (x / y)].

      [x // y] is [0] for any non-positive [x].

      @raise Division_by_zero when [y < 1]. *)

  module Option : sig
    val v_map : def:'b -> f:('a -> 'b) -> 'a option -> 'b
    val map   : f:('a -> 'b) -> 'a option -> 'b option
    val get   : def:'a -> 'a option -> 'a
  end

  (** Addons to {!Cstruct}. *)
  module Cs : sig

    val empty : Cstruct.t
    (** [empty] is an empty [Cstruct]. *)

    val null  : Cstruct.t -> bool
    (** [null cs] tests whether [len cs = 0]. *)

    val (<+>) : Cstruct.t -> Cstruct.t -> Cstruct.t
    (** [<+>] is an alias for [Cstruct.append]. *)

    val ct_eq : Cstruct.t -> Cstruct.t -> bool
    (** Constant-time equality. *)

    val xor_into : Cstruct.t -> Cstruct.t -> int -> unit
    val xor      : Cstruct.t -> Cstruct.t -> Cstruct.t

    (** {2 Private utilities} *)

    val create : ?init:int -> int -> Cstruct.t
    val clone  : ?off:int -> ?len:int -> Cstruct.t -> Cstruct.t

    val (lsl) : Cstruct.t -> int -> Cstruct.t
    val (lsr) : Cstruct.t -> int -> Cstruct.t

    val of_hex : string -> Cstruct.t
  end

  val xd  : ?address:bool -> ?ascii:bool -> ?w:int -> unit -> Format.formatter -> Cstruct.t -> unit
  (** [xd ?address ?ascii ?w () ppf cs] pretty-prints [cs] on [ppf] using the
      traditional hexdump format.

      [~address] starts each line with its offset in [cs]. Default [true].

      [~ascii] prints (printable) bytes of [cs]. Default [false].

      [~w] bytes per line. Default [16]. *)

  val xdb : ?address:bool -> ?ascii:bool -> ?w:int -> unit -> Format.formatter -> bytes -> unit
  (** {!xd} for [bytes]. *)

  val bracket : init:(unit -> 'a) -> fini:('a -> unit) -> ('a -> 'b) -> 'b
  (** Safe acquire-use-release combinator. *)
end

(**/**)


(** Numeric utilities. *)
module Numeric : sig

  type bits = int

  (** Augmented numeric type.

      Includes basic common numeric ops, range of conversions to and from
      variously-sized int types, and a few basic function for representing such
      numbers as {!Cstruct.t}. *)
  module type S = sig

    (** {1 Base}

        Type [t] with the basic bit-twiddling related operations. *)

    type t

    val zero : t
    val one  : t

    val (lsr)  : t -> int -> t
    val (lsl)  : t -> int -> t
    val (land) : t -> t -> t
    val (lor)  : t -> t -> t
    val (lxor) : t -> t -> t

    val (+)  : t -> t -> t
    val (-)  : t -> t -> t

    val succ : t -> t
    val pred : t -> t

    (** {1 Conversion} *)

    val of_int   : int -> t
    val of_int32 : int32 -> t
    val of_int64 : int64 -> t
    val to_int   : t -> int
    val to_int32 : t -> int32
    val to_int64 : t -> int64

    (** {1 External representation} *)

    val bit_bound : t -> bits
    (** [bit_bound t] computes the upper bound of {{!bits}[bits]} quickly. *)

    val pp_print : Format.formatter -> t -> unit
    (** [pp_print ppf t] pretty-prints [t] on [ppf]. *)

    val bits : t -> bits
    (** [bits t] is the minimal number of bits needed to describe [t].

        [(2^(bits t)) / 2 <= t < 2^(bits t)]. *)

    val of_cstruct_be : ?bits:bits -> Cstruct.t -> t
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

    val to_cstruct_be : ?size:int -> t -> Cstruct.t
    (** [to_cstruct_be ~size t] is the big-endian representation of [t].

        If [~size] is not given, it defaults to the minimal number of bytes
        needed to represent [t], which is [bits t / 8] rounded up.

        The least-significant bit of [t] is always the last bit in the result.
        If the size is larger than needed, the output is padded with zero bits.
        If it is smaller, the high bits in [t] are dropped. *)

    val into_cstruct_be : t -> Cstruct.t -> unit
    (** [into_cstruct_be t cs] writes the big-endian representation of [t] into
        [cs]. It behaves like {{!to_cstruct_be}[to_cstruct_be]}, with [~size]
        spanning the entire [cs]. *)
  end

  module Int   : S with type t = int
  module Int32 : S with type t = int32
  module Int64 : S with type t = int64
  module Z     : S with type t = Z.t

  (** {1 Misc elementary number theory} *)

  val pseudoprime : Z.t -> bool
  (** Miller-Rabin with sane rounds parameter. *)

end


(** {1 Hashing} *)

(** Hashes.

    Each algorithm is contained in its own {{!hashing_modules}module}, with
    high-level operations accessible through {{!hashing_funs}functions} that
    dispatch on {{!hash}code} value. *)
module Hash : sig

  type digest = Cstruct.t

  type 'a iter = ('a -> unit) -> unit
  (** A general (inner) iterator. It applies the provided function to a
      collection of elements.

      For instance:

      {ul
      {- [let iter_k    : 'a      -> 'a iter = fun x f -> f x]}
      {- [let iter_pair : 'a * 'a -> 'a iter = fun (x, y) f = f x; f y]}
      {- [let iter_list : 'a list -> 'a iter = fun xs f -> List.iter f xs]}} *)

  (** {1:hashing_modules Hashing algorithms} *)

  (** A single hash algorithm. *)
  module type S = sig

    val digest_size : int
    (** Size of digests (in bytes). *)

    (** {1 Core operations} *)

    type t
    (** Represents a running hash computation in a way suitable for appending
        inputs. *)

    val empty : t
    (** [empty] is the hash of the empty string. *)

    val feed : t -> Cstruct.t -> t
    (** [feed t msg] adds the information in [msg] to [t].

        [feed] is analogous to appending:
        [feed (feed t msg1) msg2 = feed t (append msg1 msg2)]. *)

    val get : t -> digest
    (** [get t] is the digest corresponding to [t]. *)

    (** {1 All-in-one}

        Functions that operate on data stored in a single chunk. *)

    val digest : Cstruct.t -> digest
    (** [digest msg] is the digest of [msg].

        [digest msg = get (feed empty msg)] *)

    val hmac : key:Cstruct.t -> Cstruct.t -> digest
    (** [hmac ~key bytes] is the authentication code for [bytes] under the
        secret [key], generated using the standard HMAC construction over this
        hash algorithm. *)

    (** {1:hashing_funs Functions over iterators}

        Functions that operate on arbitrary {{!iter}iterators}. They can serve
        as a basis for other, more specialized aggregate hashing operations.

        These functions are a little faster than using {{!feed}[feed]} directly. *)

    val feedi : t -> Cstruct.t iter -> t
    (** [feedi t iter =
  (let r = ref t in iter (fun msg -> r := feed !r msg); !r)] *)

    val digesti : Cstruct.t iter -> digest
    (** [digesti iter = feedi empty iter |> get] *)

    val hmaci : key:Cstruct.t -> Cstruct.t iter -> digest
    (** See {{!hmac}[hmac]}. *)
  end

  module MD5     : S
  module SHA1    : S
  module SHA224  : S
  module SHA256  : S
  module SHA384  : S
  module SHA512  : S

  (** {1 Codes-based interface} *)

  type hash = [ `MD5 | `SHA1 | `SHA224 | `SHA256 | `SHA384 | `SHA512 ]
  (** Algorithm codes.

      {e [Sexplib] convertible}. *)

  val module_of   : [< hash ] -> (module S)
  (** [module_of hash] is the (first-class) module corresponding to the code
      [hash].

      This is the most convenient way to go from a code to a module. *)

  val digest      : [< hash ] -> Cstruct.t -> digest
  val digesti     : [< hash ] -> Cstruct.t iter -> digest
  val mac         : [< hash ] -> key:Cstruct.t -> Cstruct.t -> digest
  val maci        : [< hash ] -> key:Cstruct.t -> Cstruct.t iter -> digest
  val digest_size : [< hash ] -> int

  (** {1 Misc} *)

  type 'a or_digest = [ `Message of 'a | `Digest of digest ]
  (** Either an ['a] or its digest, according to some hash algorithm. *)

  (**/**)
  val hash_of_sexp : Sexplib.Sexp.t -> hash
  val sexp_of_hash : hash -> Sexplib.Sexp.t
  (**/**)

end


(** {1 Symmetric-key cryptography} *)

(** Block ciphers.

    Each algorithm, and each mode of operation, is contained in its own separate
    module. *)
module Cipher_block : sig

  (** Module types for various block cipher modes of operation. *)
  module S : sig

    (** Raw block cipher in all its glory.

        Make absolutely sure to check the arguments. Behavior is unspecified on
        invalid inputs. *)
    (* module type Core = sig *)

    (*   type ekey *)
    (*   type dkey *)

    (*   val of_secret   : Cstruct.t -> ekey * dkey *)
    (*   val e_of_secret : Cstruct.t -> ekey *)
    (*   val d_of_secret : Cstruct.t -> dkey *)

    (*   val key   : int array *)
    (*   val block : int *)

    (*   val encrypt : key:ekey -> blocks:int -> Native.buffer -> int -> Native.buffer -> int -> unit *)
    (*   val decrypt : key:dkey -> blocks:int -> Native.buffer -> int -> Native.buffer -> int -> unit *)
    (* end *)

    (** Modes of operation: *)

    (** {e Electronic Codebook} "mode". *)
    module type ECB = sig

      type key
      val of_secret : Cstruct.t -> key

      val key_sizes  : int array
      val block_size : int
      val encrypt : key:key -> Cstruct.t -> Cstruct.t
      val decrypt : key:key -> Cstruct.t -> Cstruct.t
    end

    (** {e Cipher-block chaining} mode. *)
    module type CBC = sig

      type key

      val of_secret : Cstruct.t -> key
      (** Construct the encryption key corresponding to [secret].

          @raise Invalid_argument if the length of [secret] is not in
          {{!key_sizes}[key_sizes]}. *)

      val key_sizes : int array
      (** Key sizes allowed with this cipher. *)

      val block_size : int
      (** The size of a single block. *)

      val encrypt : key:key -> iv:Cstruct.t -> Cstruct.t -> Cstruct.t
      (** [encrypt ~key ~iv msg] is [msg] encrypted under [key], using [iv] as
          the CBC initialization vector.

          @raise Invalid_argument if [iv] is not [block_size], or [msg] is not
          [k * block_size] long. *)

      val decrypt : key:key -> iv:Cstruct.t -> Cstruct.t -> Cstruct.t
      (** [decrypt ~key ~iv msg] is the inverse of [encrypt].

          @raise Invalid_argument if [iv] is not [block_size], or [msg] is not
          [k * block_size] long. *)

      val next_iv : iv:Cstruct.t -> Cstruct.t -> Cstruct.t
      (** [next_iv ~iv ciphertext] is the first [iv] {e following} the
          encryption that used [iv] to produce [ciphertext].

          For protocols which perform inter-message chaining, this is the [iv]
          for the next message.

          It is either [iv], when [len ciphertext = 0], or the last block of
          [ciphertext]. Note that

{[encrypt ~iv msg1 || encrypt ~iv:(next_iv ~iv (encrypt ~iv msg1)) msg2
  == encrypt ~iv (msg1 || msg2)]}

          @raise Invalid_argument if the length of [iv] is not [block_size], or
          the length of [ciphertext] is not [k * block_size] for some [k]. *)
    end

    (** {e Counter} mode. *)
    module type CTR = sig

      type key

      val of_secret : Cstruct.t -> key
      (** Construct the encryption key corresponding to [secret].

          @raise Invalid_argument if the length of [secret] is not in
          {{!key_sizes}[key_sizes]}. *)

      val key_sizes : int array
      (** Key sizes allowed with this cipher. *)

      val block_size : int
      (** The size of a single block. *)

      type ctr

      val stream : key:key -> ctr:ctr -> int -> Cstruct.t
      (** [stream ~key ~ctr n] is the raw keystream.

          Keystream is the concatenation of successive encrypted counter states.
          If [E(x)] is the single block [x] encrypted under [key], then keystream
          is the first [n] bytes of
          [E(ctr) || E(add ctr 1) || E(add ctr 2) || ...].

          Note that

{[stream ~key ~ctr (k * block_size) || stream ~key ~ctr:(add ctr k) x
  == stream ~key ~ctr (k * block_size + x)]}

          In other words, it is possible to restart a keystream at [block_size]
          boundaries by manipulating the counter. *)

      val encrypt : key:key -> ctr:ctr -> Cstruct.t -> Cstruct.t
      (** [encrypt ~key ~ctr msg] is
          [stream ~key ~ctr ~off (len msg) lxor msg]. *)

      val decrypt : key:key -> ctr:ctr -> Cstruct.t -> Cstruct.t
      (** [decrypt] is [encrypt]. *)

      val add_ctr : ctr -> int64 -> ctr

      val next_ctr : ctr:ctr -> Cstruct.t -> ctr
      (** [next_ctr ~ctr msg] is the state of the counter after encrypting or
          decrypting [msg] with the counter [ctr].

          For protocols which perform inter-message chaining, this is the
          counter for the next message.

          It is computed as [C.add ctr (ceil (len msg / block_size))]. Note that
          if [len msg1 = k * block_size],

{[encrypt ~ctr msg1 || encrypt ~ctr:(next_ctr ~ctr msg1) msg2
  == encrypt ~ctr (msg1 || msg2)]}

          *)

      val ctr_of_cstruct : Cstruct.t -> ctr
    end

    (** {e Galois/Counter Mode}. *)
    module type GCM = sig

      type key

      type result = { message : Cstruct.t ; tag : Cstruct.t }
      (** The transformed message, packed with the authentication tag. *)

      val of_secret : Cstruct.t -> key
      (** Construct the encryption key corresponding to [secret].

          @raise Invalid_argument if the length of [secret] is not in
          {{!key_sizes}[key_sizes]}. *)

      val key_sizes  : int array
      (** Key sizes allowed with this cipher. *)

      val block_size : int
      (** The size of a single block. *)

      val encrypt : key:key -> iv:Cstruct.t -> ?adata:Cstruct.t -> Cstruct.t -> result
      (** [encrypt ~key ~iv ?adata msg] is the {{!result}[result]} containing
          [msg] encrypted under [key], with [iv] as the initialization vector,
          and the authentication tag computed over both [adata] and [msg]. *)

      val decrypt : key:key -> iv:Cstruct.t -> ?adata:Cstruct.t -> Cstruct.t -> result
      (** [decrypt ~key ~iv ?adata msg] is the result containing the inversion
          of [encrypt] and the same authentication tag. *)
    end

    (** {e Counter with CBC-MAC} mode. *)
    module type CCM = sig

      type key

      val of_secret : maclen:int -> Cstruct.t -> key
      (** Construct the encryption key corresponding to [secret], that will
          produce authentication codes with the length [maclen].

          @raise Invalid_argument if the length of [secret] is not in
          {{!key_sizes}[key_sizes]} or [maclen] is not in [mac_sizes] *)

      val key_sizes  : int array
      (** Key sizes allowed with this cipher. *)

      val block_size : int
      (** The size of a single block. *)

      val mac_sizes  : int array
      (** [MAC] lengths allowed with this cipher. *)

      val encrypt : key:key -> nonce:Cstruct.t -> ?adata:Cstruct.t -> Cstruct.t -> Cstruct.t
      (** [encrypt ~key ~nonce ?adata msg] is [msg] encrypted under [key] and
          [nonce], packed with authentication data computed over [msg] and
          [adata].

          @raise Invalid_argument if [nonce] is not between 7 and 13 bytes long.  *)

      val decrypt : key:key -> nonce:Cstruct.t -> ?adata:Cstruct.t -> Cstruct.t -> Cstruct.t option
      (** [decrypt ~key ~nonce ?adata msg] is [Some text] when [msg] was
          produced by the corresponding [encrypt], or [None] otherwise.

          @raise Invalid_argument if [nonce] is not between 7 and 13 bytes long.  *)
    end
  end

  module AES : sig
(*     module Core : S.Core *)
    module ECB  : S.ECB
    module CBC  : S.CBC
    module CTR  : S.CTR with type ctr = int64 * int64
    module GCM  : S.GCM
    module CCM  : S.CCM
  end

  module DES : sig
(*     module Core : S.Core *)
    module ECB  : S.ECB
    module CBC  : S.CBC
    module CTR  : S.CTR with type ctr = int64
  end

  val accelerated : [`XOR | `AES | `GHASH] list
  (** Operations using non-portable, hardware-dependent implementation in
      this build of the library. *)
end


(** Streaming ciphers. *)
module Cipher_stream : sig

  (** General stream cipher type. *)
  module type S = sig
    type key
    type result = { message : Cstruct.t ; key : key }
    val of_secret : Cstruct.t -> key
    val encrypt : key:key -> Cstruct.t -> result
    val decrypt : key:key -> Cstruct.t -> result
  end

  (** {e Alleged Rivest Cipher 4}. *)
  module ARC4 : S
end


(** {1 Randomness} *)

(** Secure random number generation.

    There are several parts of this module:

    {ul
    {- The {{!Rng.S.Generator}signature} of generator modules, together with a
       facility to convert such modules into actual {{!g}generators}, and
       functions that operate on this representation.}
    {- A global generator instance, implemented by
       {{!Rng.Generators.Fortuna}Fortuna}.  This is the default generator, used
       when one is not explicitly supplied.}
    {- The {{!S.N}signature} of modules for randomly generating a particular
       numeric type, a {{!Rng.Make_N}functor} to produce them, and instances for
       {{!Rng.Int}[int]}, {{!Rng.Int32}[int32]}, {{!Rng.Int64}[int64]}, and
       {{!Rng.Z}[Z.t]}.}
    {- Several specialized functions for e.g. primes.}}
*)
module Rng : sig

  (** {1 Usage notes} *)

  (** {b TL;DR} Don't forget to seed; don't maintain your own [g].

      The RNGs here are merely the deterministic part of a full random number
      generation suite. For proper operation, they need to be seeded with a
      high-quality entropy source.

      Suitable entropy sources are provided by sub-libraries
      {{!Nocrypto_entropy_unix}nocrypto.unix}, {{!Nocrypto_entropy_lwt}nocrypto.lwt}
      and {{!Nocrypto_entropy_mirage}nocrypto.xen}. Although this module exposes a more
      fine-grained interface, allowing manual seeding of generators, this is intended
      either for implementing entropy-harvesting modules, or very specialized
      purposes. Users of this library should almost certainly use one of the above
      entropy libraries, and avoid manually managing the generator seeding.

      Similarly, although it is possible to swap the default generator and gain
      control over the random stream, this is also intended for specialized
      applications such as testing or similar scenarios where the RNG needs to be
      fully deterministic, or as a component of deterministic algorithms which
      internally rely on pseudorandom streams.

      In the general case, users should not maintain their local instances of
      {{!g}g}. All of the generators in a process have to compete for entropy, and
      it is likely that the overall result will have lower effective
      unpredictability.

      The recommended way to use these functions is either to accept an optional
      generator and pass it down, or to ignore the generator altogether, as
      illustrated in the {{!rng_examples}examples}.
  *)

  type bits = int

  (** {1 Interface} *)

  type g
  (** A generator (PRNG) with its state. *)

  exception Unseeded_generator
  (** Thrown when using an uninitialized {{!g}generator}. *)

  (** Module signatures. *)
  module S : sig

    (** A single PRNG algorithm. *)
    module type Generator = sig

      type g
      (** State type for this generator. *)

      val block : int
      (** Internally, this generator's {{!generate}generate} always produces
          [k * block] bytes. *)

      val create : unit -> g
      (** Create a new, unseeded {{!g}g}. *)

      val generate : g:g -> int -> Cstruct.t
      (** [generate ~g n] produces [n] uniformly distributed random bytes,
          updating the state of [g]. *)

      val reseed : g:g -> Cstruct.t -> unit
      (** [reseed ~g bytes] directly updates [g]. Its new state depends both on
          [bytes] and the previous state.

          A generator is seded after a single application of [reseed]. *)

      val accumulate : g:g -> [`Acc of source:int -> Cstruct.t -> unit]
      (** [accumulate ~g] is a closure suitable for incrementally feeding
          small amounts of environmentally sourced entropy into [g].

          Its operation should be fast enough for repeated calling from e.g.
          event loops. Systems with several distinct, stable entropy sources
          should use stable [source] to distinguish their sources.

          A generator is seeded after a single application of the closure. *)

      val seeded : g:g -> bool
      (** [seeded ~g] is [true] iff operations won't throw
          {{!Unseeded_generator}Unseeded_generator}. *)
    end

    (** Typed generation of a particular numeric type. *)
    module type N = sig

      type t
      (** The type of extracted values. *)

      val gen : ?g:g -> t -> t
      (** [gen ~g n] picks a value in the interval [\[0, n - 1\]] uniformly at random. *)

      val gen_r : ?g:g -> t -> t -> t
      (** [gen_r ~g low high] picks a value from the interval [\[low, high - 1\]]
          uniformly at random. *)

      val gen_bits : ?g:g -> ?msb:bits -> bits -> t
      (** [gen_bits ~g ~msb n] picks a bit-string [n] bits long, with [msb] most
          significant bits set, and interprets it as a {{!t}t} in big-endidan.
          This yields a value in the interval
          [\[2^(n-1) + ... + 2^(n-msb), 2^n - 1\]].

          [msb] defaults to [0] which reduces [gen_bits k] to [gen 2^k]. *)
    end

  end

  (** Ready-to-use RNG algorithms. *)
  module Generators : sig

    (** {b Fortuna}, a CSPRNG {{: https://www.schneier.com/fortuna.html} proposed}
        by Schneier. *)
    module Fortuna : S.Generator

    (** {b HMAC_DRBG}: A NIST-specified RNG based on HMAC construction over the
        provided hash. *)
    module Hmac_drgb : sig
      module Make (H : Hash.S) : S.Generator
    end

    (** No-op generator returning exactly the bytes it was seeded with. *)
    module Null : S.Generator
  end


  val create : ?g:'a -> ?seed:Cstruct.t -> ?strict:bool -> (module S.Generator with type g = 'a) -> g
  (** [create module] uses a module conforming to the {{!S.Generator}Generator}
      signature to instantiate the generic generator {{!g}g}.

      [g] is the state to use, otherwise a fresh one is created.

      [seed] can be provided to immediately reseed the generator with.

      [strict] puts the generator into a more standards-conformant, but slighty
      slower mode. Useful if the outputs need to match published test-vectors. *)

  val generator : g ref
  (** Default generator. Functions in this module use this generator when not
      explicitly supplied one.

      Swapping the [generator] is a way to subvert the random-generation process
      e.g. to make it fully deterministic.

      [generator] defaults to {{!Generators.Fortuna}Fortuna}. *)

  val generate : ?g:g -> int -> Cstruct.t
  (** Invoke {{!S.Generator.generate}generate} on [g] or
      {{!generator}default generator}. *)

  val block : g option -> int
  (** {{!S.Generator.block}Block} size of [g] or
      {{!generator}default generator}. *)

  (**/**)

  (* The following functions expose the seeding interface. They are meant to
   * connect the RNG with entropy-providing libraries and subject to change.
   * Client applications should not use them directly. *)

  val reseed     : ?g:g -> Cstruct.t -> unit
  val accumulate : g option -> [`Acc of source:int -> Cstruct.t -> unit]
  val seeded     : g option -> bool
  (**/**)


  (** {1 Generation of common numeric types} *)

  module Make_N (N : Numeric.S) : S.N with type t = N.t
  (** Creates a suite of generating functions over a numeric type. *)

  module Int   : S.N with type t = int
  module Int32 : S.N with type t = int32
  module Int64 : S.N with type t = int64
  module Z     : S.N with type t = Z.t


  (** {1 Specialized generation} *)

  val prime : ?g:g -> ?msb:bits -> bits -> Z.t
  (** [prime ~g ~msb bits] generates a prime smaller than [2^bits], with [msb]
      most significant bits set.

      [prime ~g ~msb:1 bits] (the default) yields a prime in the interval
      [\[2^(bits - 1), 2^bits - 1\]]. *)

  val safe_prime : ?g:g -> bits -> Z.t * Z.t
  (** [safe_prime ~g bits] gives a prime pair [(g, p)] such that [p = 2g + 1]
      and [p] has [bits] significant bits. *)

  (** {1:rng_examples Examples}

      Generating a random 13-byte {!Cstruct.t}:
{[let cs = Rng.generate 13]}

      Generating a list of {!Cstruct.t}s, passing down an optional
      {{!g}generator}:
{[let rec f1 ?g ~n i =
  if i < 1 then [] else Rng.generate ?g n :: f1 ?g ~n (i - 1)]}

      Generating a [Z.t] smaller than [10] and an [int64] in the range [\[3, 7\]]:
{[let f2 ?g () = Rng.(Z.gen ?g ~$10, Int64.gen_r 3L 8L)]}

      Creating a local Fortuna instance and using it as a key-derivation function:
{[let f3 secret =
  let g = Rng.(create ~seed:secret (module Generators.Fortuna)) in
  Rng.generate ~g 32]}

      Generating a 17-bit prime with two leading bits set:
{[let p = Rng.prime ~msb:2 17]}

      Fisher-Yates shuffle:
{[let f4 ?g arr =
  let n = Array.length arr in
  arr |> Array.iter @@ fun i ->
    let j = Rng.Int.gen_r ?g i n in
    let (a, b) = (arr.(i), arr.(j)) in
    arr.(i) <- b ; arr.(j) <- a ]}
      *)

  type buffer = Cstruct.t
  (** Type definition to satisfy MirageOS RANDOM signature *)
end


(** {1 Public-key cryptography} *)

(** {b RSA} public-key cryptography.

Keys are taken to be trusted material, and their properties are not checked.

Messages are checked not to exceed the key size, and this is signalled via
exceptions.

Private-key operations are optionally protected through RSA blinding. *)
module Rsa : sig

  type bits = int

  (** {1 Keys}

      {b Warning} The behavior of functions in this module is undefined if the
      key material is not numerically well-formed. It is the responsibility of
      the client to ensure the trustworthiness of keys.

      The two anchoring points provided are {{!generate}[generate]} and
      {{!well_formed}[well_formed]}. *)

  exception Insufficient_key
  (** Raised if the key is too small to transform the given message, i.e. if the
      numerical interpretation of the (potentially padded) message is not
      smaller than the modulus. *)

  type pub  = {
    e : Z.t ; (** Public exponent *)
    n : Z.t ; (** Modulus *)
  }
  (** The public portion of the key.

      {e [Sexplib] convertible}. *)

  type priv = {
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

  val pub_bits : pub -> bits
  (** Bit-size of a public key. *)

  val priv_bits : priv -> bits
  (** Bit-size of a private key. *)

  val priv_of_primes : e:Z.t -> p:Z.t -> q:Z.t -> priv
  (** [priv_of_primes ~e ~p ~q] is the {{!priv}private key} derived from the
      minimal description [(e, p, q)].

      The triple is not checked for well-formedness.

      [p] is assumed to be the smaller factor. While the key will function
      correctly in either case, derived quantities will be different. See
      {{!priv} private keys}. *)

  val priv_of_exp : ?g:Rng.g -> ?attempts:int -> e:Z.t -> d:Z.t -> Z.t -> priv
  (** [priv_of_exp ?g ?attempts ~e ~d n] is the unique {{!priv}private key}
      characterized by the public ([e]) and private ([d]) exponents, and modulus
      [n]. This operation uses a probabilistic process that can fail to recover
      the key.

      [~attempts] is the number of trials. For triplets that form an RSA key,
      the probability of failure is at most [2^(-attempts)]. [attempts] defaults
      to an unspecified number that yields a very high probability of recovering
      valid keys.

      @raise Invalid_argument when [(e, d, n)] certainly do not form an RSA key.
      This includes violating [2 < e < n], [2 < d < n] or [2 < n].

      @raise Failure when the key has not been recovered after the given number
      of attempts. *)

  val pub_of_priv : priv -> pub
  (** Extract the public component from a private key. *)

  val well_formed : e:Z.t -> p:Z.t -> q:Z.t -> bool
  (** [well_formed ~e ~p ~q] indicates whether the triplet [(e, p, q)] can be
      used as an RSA key.

      It can, if:
      {ul
      {- [3 <= e];}
      {- [p != q];}
      {- [e], [p] and [q] are all primes; and}
      {- [e] is not a divisor of either [p - 1] or [q - 1].}}

      These are sufficient conditions to ensure that the behavior of other
      operations in this module is defined.

      This will not help with maliciously crafted keys that are simply
      numerically well-formed, however. Carefully consider which sources of keys
      to trust. *)

  (** {1 The RSA transformation} *)

  type mask = [ `No | `Yes | `Yes_with of Rng.g ]
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

  val decrypt : ?mask:mask -> key:priv -> Cstruct.t -> Cstruct.t
  (** [decrypt ~mask key ciphertext] is the decrypted [ciphertext], left-padded
      with [0x00] up to [key] size.

      [~mask] defaults to [`Yes].

      @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key}) *)

  (** {1 Key generation} *)

  val generate : ?g:Rng.g -> ?e:Z.t -> bits -> priv
  (** [generate g e bits] is a new {{!priv}private key}. The new key is
      guaranteed to be {{!well_formed}well formed}.

      [e] defaults to [2^16+1].

      {b Note} This process might diverge if there are no keys for the given
      bit size. This can happen when [bits] is extremely small.

      @raise Invalid_argument if [e] is not prime [3 <= e < 2^bits]. *)


  (** {1 PKCS#1 padded modes} *)

  (** {b PKCS v1.5} operations, as defined by {b PKCS #1 v1.5}.

      For the operations that only add the raw padding, the key size must be at
      least 11 bytes larger than the message. For full {{!sign}signing}, the
      minimal key size varies according to the hash algorithm. In this case, the
      key size is [priv_bits key / 8], rounded up. *)
  module PKCS1 : sig

    val encrypt : ?g:Rng.g -> key:pub -> Cstruct.t -> Cstruct.t
    (** [encrypt g key message] is a PKCS1-padded (type 2) and encrypted
        [message].

        @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key}) *)

    val decrypt : ?mask:mask -> key:priv -> Cstruct.t -> Cstruct.t option
    (** [decrypt mask key ciphertext] is [Some message] if the [ciphertext] was
        produced by the corresponding {{!encrypt}encrypt} operation, or [None]
        otherwise. *)

    val sig_encode : ?mask:mask -> key:priv -> Cstruct.t -> Cstruct.t
    (** [sig_encode ?mask ~key message] is the PKCS1-padded (type 1) [message]
        signed by the [key].

        {b Note} This operation performs only the padding and RSA transformation
        steps of the PKCS 1.5 signature. The full signature is implemented by
        {{!sign}[sign]}.

        @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key}) *)

    val sig_decode : key:pub -> Cstruct.t -> Cstruct.t option
    (** [sig_decode key signature] is [Some message] when the [signature]
        was produced with the given [key] as per {{!sig_encode}sig_encode}, or
        [None] *)

    open Hash

    val min_key : hash -> bits
    (** [min_key hash] is the minimum key size required by {{!sign}[sign]}. *)

    val sign : ?mask:mask -> hash:hash -> key:priv -> Cstruct.t or_digest -> Cstruct.t
    (** [sign ?mask ~hash ~key message] is the PKCS 1.5 signature of
        [message], signed by the [key], using the hash function [hash]. This is
        the full signature, with the ASN-encoded message digest as the payload.

        [message] is either the actual message, or its digest.

        @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key})

        @raise Invalid_argument if message is a [`Digest] of the wrong size.  *)

    val verify : hashp:(hash -> bool) -> key:pub -> signature:Cstruct.t -> Cstruct.t or_digest -> bool
    (** [verify ~hashp ~key ~signature message] checks that [signature] is the
        PKCS 1.5 signature of the [message] under the given [key].

        [message] is either the actual message, or its digest.

        [hashp] determines the allowed hash algorithms. Whenever [hashp] is
        [false], [verify] is also [false].

        @raise Invalid_argument if message is a [`Digest] of the wrong size.  *)
  end

  (** {b OAEP}-padded encryption, as defined by {b PKCS #1 v2.1}.

      The same hash function is used for padding and MGF. MGF is {b MGF1} as
      defined in {b PKCS #1 2.1}.

      Keys must have a minimum of [2 + 2 * hlen + len(message)] bytes, where
      [hlen] is the hash length. *)
  module OAEP (H : Hash.S) : sig

    val encrypt : ?g:Rng.g -> ?label:Cstruct.t -> key:pub -> Cstruct.t -> Cstruct.t
    (** [encrypt ~g ~label ~key message] is {b OAEP}-padded and encrypted
        [message], using the optional [label].

        @raise Insufficient_key (see {{!Insufficient_key}Insufficient_key}) *)

    val decrypt : ?mask:mask -> ?label:Cstruct.t -> key:priv -> Cstruct.t -> Cstruct.t option
    (** [decrypt ~mask ~label ~key ciphertext] is [Some message] if the
        [ciphertext] was produced by the corresponding {{!encrypt}encrypt}
        operation, or [None] otherwise. *)
  end

  (** {b PSS}-based signing, as defined by {b PKCS #1 v2.1}.

      The same hash function is used for padding, MGF and computing message
      digest. MGF is {b MGF1} as defined in {b PKCS #1 2.1}.

      Keys must have a minimum of [2 + hlen + slen] bytes, where [hlen] is the
      hash length and [slen] is the seed length. *)
  module PSS (H: Hash.S) : sig

    open Hash

    val sign : ?g:Rng.g -> ?mask:mask -> ?slen:int -> key:priv -> Cstruct.t or_digest -> Cstruct.t
    (** [sign ~g ~mask ~slen ~key message] the {p PSS}-padded digest of
        [message], signed with the [key].

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

  type bits = int

  (** {1 DSA signature algorithm} *)

  type priv = {
    p  : Z.t ; (** Modulus *)
    q  : Z.t ; (** Subgroup order *)
    gg : Z.t ; (** Group Generator *)
    x  : Z.t ; (** Private key proper *)
    y  : Z.t ; (** Public component *)
  }
  (** Private key. [p], [q] and [gg] comprise {i domain parameters}.

      {e [Sexplib] convertible}. *)

  type pub  = {
    p  : Z.t ;
    q  : Z.t ;
    gg : Z.t ;
    y  : Z.t ;
  }
  (** Public key, a subset of {{!priv}private key}.

      {e [Sexplib] convertible}. *)

  type keysize = [ `Fips1024 | `Fips2048 | `Fips3072 | `Exactly of bits * bits ]
  (** Key size request. Three {e Fips} variants refer to FIPS-standardized
      L-values ([p] size) and imply the corresponding N ([q] size); The last
      variants specifies L and N directly. *)

  type mask = [ `No | `Yes | `Yes_with of Rng.g ]
  (** Masking (cryptographic blinding) option. *)

  val pub_of_priv : priv -> pub
  (** Extract the public component from a private key. *)

  val generate : ?g:Rng.g -> keysize -> priv
  (** [generate g size] is a fresh {{!priv}private} key. The domain parameters
      are derived using a modified FIPS.186-4 probabilistic process, but the
      derivation can not be validated.

      {b Note} The process might diverge if it is impossible to find parameters
      with the given bit sizes. This happens when [n] gets too big for [l], if
      the [size] was given as [`Exactly (l, n)].

      @raise Invalid_argument if [size] is (`Exactly (l, n)), and either [l] or
      [n] is ridiculously small. *)

  val sign : ?mask:mask -> ?k:Z.t -> key:priv -> Cstruct.t -> Cstruct.t * Cstruct.t
  (** [sign mask k fips key digest] is the signature, a pair of {!Cstruct.t}s
      representing [r] and [s] in big-endian.

      [digest] is the full digest of the actual message.

      [k], the random component, can either be provided, or is deterministically
      derived as per RFC6979, using SHA256.  *)

  val verify : key:pub -> Cstruct.t * Cstruct.t -> Cstruct.t -> bool
  (** [verify fips key (r, s) digest] verifies that the pair [(r, s)] is the signature
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
  module K_gen (H : Hash.S) : sig

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

  type bits = int

  (** {1 Diffie-Hellman key exchange} *)

  exception Invalid_public_key
  (** Raised if the public key is degenerate. Implies either badly malfunctioning
      DH on the other side, or an attack attempt. *)

  type group = {
    p  : Z.t ;        (** modulus *)
    gg : Z.t ;        (** generator *)
    q  : Z.t option ; (** subgroup order; potentially unknown *)
  }
  (** A DH group.

      {e [Sexplib] convertible}. *)

  type secret = private { x : Z.t }
  (** A private secret.

      {e [Sexplib] convertible.} *)

  val modulus_size : group -> bits
  (** Bit size of the modulus. *)

  val key_of_secret : group -> s:Cstruct.t -> secret * Cstruct.t
  (** [key_of_secret group s] is the {!secret} and the corresponding public
      key which use [s] as the secret exponent.

      @raise Invalid_public_key if [s] is degenerate. *)

  val gen_key : ?g:Rng.g -> ?bits:bits -> group -> secret * Cstruct.t
  (** Generate a random {!secret} and the corresponding public key.
      [bits] is the exact bit-size of {!secret} and defaults to a value
      dependent on the {!group}'s [p].

      {b Note} The process might diverge when [bits] is extremely small. *)

  val shared : group -> secret -> Cstruct.t -> Cstruct.t option
  (** [shared group secret message] is [Some key], the shared key, given a
      group, a previously generated {!secret} and the other party's public
      message. It is [None] if [message] is degenerate. *)

  val gen_group : ?g:Rng.g -> bits -> group
  (** [gen_group bits] generates a random {!group} with modulus size [bits].
      Uses a safe prime [p = 2q + 1] (with [q] prime) for the modulus and [2]
      for the generator, such that [2^q = 1 mod p].
      Runtime is on the order of minute for 1024 bits.

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

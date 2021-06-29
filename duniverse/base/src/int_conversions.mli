(** Conversions between various integer types *)

open! Import

(** Ocaml has the following integer types, with the following bit widths
    on 32-bit and 64-bit architectures.

    {v
                            arch  arch
                type        32b   64b
                ----------------------
                int          31    63  (32 when compiled to JavaScript)
                nativeint    32    64
                int32        32    32
                int64        64    64
       v}

    In both cases, the following inequalities hold:

    {[
      width(int) < width(nativeint)
      && width(int32) <= width(nativeint) <= width(int64)
    ]}

    The conversion functions come in one of two flavors.

    If width(foo) <= width(bar) on both 32-bit and 64-bit architectures, then we have

    {[ val foo_to_bar : foo -> bar ]}

    otherwise we have

    {[
      val foo_to_bar     : foo -> bar option
      val foo_to_bar_exn : foo -> bar
    ]} *)
val int_to_int32 : int -> int32 option

val int_to_int32_exn : int -> int32
val int_to_int32_trunc : int -> int32
val int_to_int64 : int -> int64
val int_to_nativeint : int -> nativeint
val int32_to_int : int32 -> int option
val int32_to_int_exn : int32 -> int
val int32_to_int_trunc : int32 -> int
val int32_to_int64 : int32 -> int64
val int32_to_nativeint : int32 -> nativeint
val int64_to_int : int64 -> int option
val int64_to_int_exn : int64 -> int
val int64_to_int_trunc : int64 -> int
val int64_to_int32 : int64 -> int32 option
val int64_to_int32_exn : int64 -> int32
val int64_to_int32_trunc : int64 -> int32
val int64_to_nativeint : int64 -> nativeint option
val int64_to_nativeint_exn : int64 -> nativeint
val int64_to_nativeint_trunc : int64 -> nativeint
val int64_fit_on_int63_exn : int64 -> unit
val int64_is_representable_as_int63 : int64 -> bool
val nativeint_to_int : nativeint -> int option
val nativeint_to_int_exn : nativeint -> int
val nativeint_to_int_trunc : nativeint -> int
val nativeint_to_int32 : nativeint -> int32 option
val nativeint_to_int32_exn : nativeint -> int32
val nativeint_to_int32_trunc : nativeint -> int32
val nativeint_to_int64 : nativeint -> int64
val num_bits_int : int
val num_bits_int32 : int
val num_bits_int64 : int
val num_bits_nativeint : int

(** human-friendly string (and possibly sexp) conversions *)
module Make (I : sig
    type t

    val to_string : t -> string
  end) : sig
  val to_string_hum : ?delimiter:char (** defaults to ['_'] *) -> I.t -> string
  val sexp_of_t : I.t -> Sexp.t
end

(** in the output, [to_string], [of_string], [sexp_of_t], and [t_of_sexp] convert
    between [t] and signed hexadecimal with an optional "0x" or "0X" prefix. *)
module Make_hex (I : sig
    type t [@@deriving_inline compare, hash]

    val compare : t -> t -> int
    val hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
    val hash : t -> Ppx_hash_lib.Std.Hash.hash_value

    [@@@end]


    (** [to_string] and [of_string] convert between [t] and unsigned,
        unprefixed hexadecimal.
        They must be able to handle all non-negative values and also
        [min_value]. [to_string min_value] must write a positive hex
        representation. *)
    val to_string : t -> string

    val of_string : string -> t
    val zero : t
    val ( < ) : t -> t -> bool
    val neg : t -> t
    val module_name : string
  end) : Int_intf.Hexable with type t := I.t

(** global ref affecting whether the [sexp_of_t] returned by [Make]
    is consistent with the [to_string] input or the [to_string_hum] output *)
val sexp_of_int_style : [ `No_underscores | `Underscores ] ref

(** utility for defining to_string_hum on numeric types -- takes a string matching
    (-|+)?[0-9a-fA-F]+ and puts [delimiter] every [chars_per_delimiter] characters
    starting from the right. *)
val insert_delimiter_every
  :  string
  -> delimiter:char
  -> chars_per_delimiter:int
  -> string

(** [insert_delimiter_every ~chars_per_delimiter:3] *)
val insert_delimiter : string -> delimiter:char -> string

(** [insert_delimiter ~delimiter:'_'] *)
val insert_underscores : string -> string

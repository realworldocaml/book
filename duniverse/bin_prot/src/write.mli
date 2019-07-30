(** Writing values to the binary protocol using (mostly) OCaml. *)

open Common

type 'a writer = buf -> pos : pos -> 'a -> pos
(** Type of writer functions for the binary protocol. They take a buffer,
    a write position and a value, and return the next position after
    writing out the value. *)

type ('a, 'b) writer1 = 'a writer -> 'b writer
type ('a, 'b, 'c) writer2 = 'a writer -> ('b, 'c) writer1
type ('a, 'b, 'c, 'd) writer3 = 'a writer -> ('b, 'c, 'd) writer2

val bin_write_unit : unit writer
val bin_write_bool : bool writer
val bin_write_string : string writer
val bin_write_bytes : bytes writer
val bin_write_char : char writer
val bin_write_int : int writer
val bin_write_nat0 : Nat0.t writer
val bin_write_float : float writer
val bin_write_int32 : int32 writer
val bin_write_int64 : int64 writer
val bin_write_nativeint : nativeint writer
val bin_write_ref : ('a, 'a ref) writer1
val bin_write_lazy : ('a, 'a lazy_t) writer1
val bin_write_option : ('a, 'a option) writer1
val bin_write_pair : ('a, 'b, 'a * 'b) writer2
val bin_write_triple : ('a, 'b, 'c, 'a * 'b * 'c) writer3
val bin_write_list : ('a, 'a list) writer1
val bin_write_array : ('a, 'a array) writer1
val bin_write_hashtbl : ('a, 'b, ('a, 'b) Hashtbl.t) writer2
val bin_write_float32_vec : vec32 writer
val bin_write_float64_vec : vec64 writer
val bin_write_vec : vec writer
val bin_write_float32_mat : mat32 writer
val bin_write_float64_mat : mat64 writer
val bin_write_mat : mat writer
val bin_write_bigstring : buf writer
val bin_write_float_array : float array writer
val bin_write_md5 : Md5_lib.t writer

val bin_write_variant_int : int writer
(** [bin_write_variant_int] writes out the exact little-endian bit
    representation of the variant tag of the given value (= 32 bits). *)

val bin_write_int_8bit : int writer
(** [bin_write_int_8bit] writes out the exact little-endian bit representation
    of the given [int] value using the lower 8 bits. *)

val bin_write_int_16bit : int writer
(** [bin_write_int_16bit] writes out the exact little-endian bit representation
    of the given [int] value using the lower 16 bits. *)

val bin_write_int_32bit : int writer
(** [bin_write_int_32bit] writes out the exact little-endian bit representation
    of the given [int] value using the lower 32 bits. *)

val bin_write_int_64bit : int writer
(** [bin_write_int_64bit] writes out the exact little-endian bit representation
    of the given [int] value using all 64 bits.  On 32bit platforms negative
    numbers will be sign-extended in the 64bit representation. *)

val bin_write_int64_bits : int64 writer
(** [bin_write_int64_bits] writes out the exact little-endian bit
    representation of the given [int64] value. *)

val bin_write_network16_int : int writer
(** [bin_write_network16_int] writes out an integer in 16bit network
    byte order (= big-endian). *)

val bin_write_network32_int : int writer
(** [bin_write_network32_int] writes out an integer in 32bit network
    byte order (= big-endian). *)

val bin_write_network32_int32 : int32 writer
(** [bin_write_network32_int32] writes out a 32bit integer in 32bit
    network byte order (= big-endian). *)

val bin_write_network64_int : int writer
(** [bin_write_network64_int] writes out an integer in 64bit network
    byte order (= big-endian). *)

val bin_write_network64_int64 : int64 writer
(** [bin_write_network64_int64] writes out a 64bit integer in 64bit
    network byte order (= big-endian). *)

val bin_write_array_no_length : ('a, 'a array) writer1
  [@@deprecated
    "[since 2016-03] this function was deprecated as it is misleading and unused"]
(** [bin_write_array_no_length] writes out all values in the given array
    without writing out its length. *)

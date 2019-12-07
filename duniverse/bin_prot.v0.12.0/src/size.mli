(** Compute size of values in the binary protocol. *)

open Common

type 'a sizer = 'a -> int
type ('a, 'b) sizer1 = 'a sizer -> 'b sizer
type ('a, 'b, 'c) sizer2 = 'a sizer -> ('b, 'c) sizer1
type ('a, 'b, 'c, 'd) sizer3 = 'a sizer -> ('b, 'c, 'd) sizer2

val bin_size_unit : unit sizer
val bin_size_bool : bool sizer
val bin_size_string : string sizer
val bin_size_bytes : bytes sizer
val bin_size_char : char sizer
val bin_size_int : int sizer
val bin_size_float : float sizer
val bin_size_int32 : int32 sizer
val bin_size_int64 : int64 sizer
val bin_size_nativeint : nativeint sizer
val bin_size_nat0 : Nat0.t sizer
val bin_size_ref : ('a, 'a ref) sizer1
val bin_size_lazy_t : ('a, 'a lazy_t) sizer1
val bin_size_lazy : ('a, 'a lazy_t) sizer1
val bin_size_option : ('a, 'a option) sizer1
val bin_size_pair : ('a, 'b, 'a * 'b) sizer2
val bin_size_triple : ('a, 'b, 'c, 'a * 'b * 'c) sizer3
val bin_size_list : ('a, 'a list) sizer1
val bin_size_array : ('a, 'a array) sizer1
val bin_size_hashtbl : ('a, 'b, ('a, 'b) Hashtbl.t) sizer2
val bin_size_float32_vec : vec32 sizer
val bin_size_float64_vec : vec64 sizer
val bin_size_vec : vec sizer
val bin_size_float32_mat : mat32 sizer
val bin_size_float64_mat : mat64 sizer
val bin_size_mat : mat sizer
val bin_size_bigstring : buf sizer
val bin_size_float_array : float array sizer
val bin_size_variant_int : int sizer
val bin_size_int_8bit : int sizer
val bin_size_int_16bit : int sizer
val bin_size_int_32bit : int sizer
val bin_size_int_64bit : int sizer
val bin_size_int64_bits : int64 sizer
val bin_size_network16_int : int sizer
val bin_size_network32_int : int sizer
val bin_size_network32_int32 : int32 sizer
val bin_size_network64_int : int sizer
val bin_size_network64_int64 : int64 sizer
val bin_size_array_no_length : ('a, 'a array) sizer1
  [@@deprecated
    "[since 2016-03] this function was deprecated as it is misleading and unused"]
val bin_size_md5 : Md5_lib.t sizer

(* Provide the maximum sizes for fields which do not depend upon an array/vector/matrix
   length, choosing the size required for the largest architecture.  This allows for the
   most conservative estimation of space required. *)
module Maximum : sig
  val bin_size_unit            : int
  val bin_size_bool            : int
  val bin_size_char            : int
  val bin_size_md5             : int
  val bin_size_int             : int
  val bin_size_float           : int
  val bin_size_int32           : int
  val bin_size_int64           : int
  val bin_size_nativeint       : int
  val bin_size_nat0            : int
  val bin_size_variant_int     : int
  val bin_size_int_8bit        : int
  val bin_size_int_16bit       : int
  val bin_size_int_32bit       : int
  val bin_size_int_64bit       : int
  val bin_size_int64_bits      : int
  val bin_size_network16_int   : int
  val bin_size_network32_int   : int
  val bin_size_network32_int32 : int
  val bin_size_network64_int   : int
  val bin_size_network64_int64 : int
end

(* Provide absolute minimum sizes for fields, choosing [0] for the lengths of any
   arrays/vectors/matrices. *)
module Minimum : sig
  val bin_size_unit            : int
  val bin_size_bool            : int
  val bin_size_string          : int
  val bin_size_bytes           : int
  val bin_size_char            : int
  val bin_size_md5             : int
  val bin_size_int             : int
  val bin_size_float           : int
  val bin_size_int32           : int
  val bin_size_int64           : int
  val bin_size_nativeint       : int
  val bin_size_nat0            : int
  val bin_size_ref             : int
  val bin_size_lazy_t          : int
  val bin_size_option          : int
  val bin_size_pair            : int
  val bin_size_triple          : int
  val bin_size_list            : int
  val bin_size_array           : int
  val bin_size_hashtbl         : int
  val bin_size_float32_vec     : int
  val bin_size_float64_vec     : int
  val bin_size_vec             : int
  val bin_size_float32_mat     : int
  val bin_size_float64_mat     : int
  val bin_size_mat             : int
  val bin_size_bigstring       : int
  val bin_size_float_array     : int
  val bin_size_variant_int     : int
  val bin_size_int_8bit        : int
  val bin_size_int_16bit       : int
  val bin_size_int_32bit       : int
  val bin_size_int_64bit       : int
  val bin_size_int64_bits      : int
  val bin_size_network16_int   : int
  val bin_size_network32_int   : int
  val bin_size_network32_int32 : int
  val bin_size_network64_int   : int
  val bin_size_network64_int64 : int
end

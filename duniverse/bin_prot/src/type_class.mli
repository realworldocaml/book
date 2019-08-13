(** Sizers, writers, and readers in records *)

open Common

type 'a writer =
  {
    size : 'a Size.sizer;
    write : 'a Write.writer;
  }

type 'a reader =
  {
    read : 'a Read.reader;
    vtag_read : (int -> 'a) Read.reader;
  }

type 'a t =
  {
    shape : Shape.t;
    writer : 'a writer;
    reader : 'a reader;
  }

type 'a writer0 = 'a writer
type 'a reader0 = 'a reader
type 'a t0 = 'a t

module S1 : sig
  type ('a, 'b) writer = 'a writer0 -> 'b writer0
  type ('a, 'b) reader = 'a reader0 -> 'b reader0
  type ('a, 'b) t = 'a t0 -> 'b t0
end

module S2 : sig
  type ('a, 'b, 'c) writer = 'a writer0 -> ('b, 'c) S1.writer
  type ('a, 'b, 'c) reader = 'a reader0 -> ('b, 'c) S1.reader
  type ('a, 'b, 'c) t = 'a t0 -> ('b, 'c) S1.t
end

module S3 : sig
  type ('a, 'b, 'c, 'd) writer = 'a writer0 -> ('b, 'c, 'd) S2.writer
  type ('a, 'b, 'c, 'd) reader = 'a reader0 -> ('b, 'c, 'd) S2.reader
  type ('a, 'b, 'c, 'd) t = 'a t0 -> ('b, 'c, 'd) S2.t
end

#define MK_BASE_TP(NAME, TP) \
  val bin_writer_##NAME : TP writer \
  val bin_reader_##NAME : TP reader \
  val bin_shape_##NAME : Shape.t \
  val bin_##NAME : TP t

#define MK_BASE(NAME) MK_BASE_TP(NAME, NAME)

#define MK_BASE1_TP(NAME, TP) \
  val bin_writer_##NAME : ('a, 'a TP) S1.writer \
  val bin_reader_##NAME : ('a, 'a TP) S1.reader \
  val bin_shape_##NAME : Shape.t -> Shape.t \
  val bin_##NAME : ('a, 'a TP) S1.t

#define MK_BASE1(NAME) MK_BASE1_TP(NAME, NAME)

#define MK_BASE2_TP(NAME, TP) \
  val bin_writer_##NAME : ('a, 'b, ('a, 'b) TP) S2.writer \
  val bin_reader_##NAME : ('a, 'b, ('a, 'b) TP) S2.reader \
  val bin_shape_##NAME : Shape.t -> Shape.t -> Shape.t \
  val bin_##NAME : ('a, 'b, ('a, 'b) TP) S2.t

#define MK_BASE2(NAME) MK_BASE2_TP(NAME, NAME)

MK_BASE(unit)
MK_BASE(bool)
MK_BASE(string)
MK_BASE(bytes)
MK_BASE(char)
MK_BASE(int)
MK_BASE(float)
MK_BASE(int32)
MK_BASE(int64)
MK_BASE(nativeint)
MK_BASE_TP(nat0, Nat0.t)

MK_BASE1(ref)
MK_BASE1_TP(lazy, lazy_t)
MK_BASE1(option)

val bin_writer_pair : ('a, 'b, 'a * 'b) S2.writer
val bin_reader_pair : ('a, 'b, 'a * 'b) S2.reader
val bin_pair : ('a, 'b, 'a * 'b) S2.t

val bin_writer_triple : ('a, 'b, 'c, 'a * 'b * 'c) S3.writer
val bin_reader_triple : ('a, 'b, 'c, 'a * 'b * 'c) S3.reader
val bin_triple : ('a, 'b, 'c, 'a * 'b * 'c) S3.t

MK_BASE1(list)
MK_BASE1(array)

MK_BASE2_TP(hashtbl, Hashtbl.t)

MK_BASE_TP(float32_vec, vec32)
MK_BASE_TP(float64_vec, vec64)
MK_BASE(vec)
MK_BASE_TP(float32_mat, mat32)
MK_BASE_TP(float64_mat, mat64)
MK_BASE(mat)
MK_BASE_TP(bigstring, buf)

type float_array = float array
MK_BASE(float_array)

val bin_writer_variant_int : int writer
val bin_reader_variant_int : int reader
val bin_variant_int : int t

MK_BASE_TP(int_8bit, int)
MK_BASE_TP(int_16bit, int)
MK_BASE_TP(int_32bit, int)
MK_BASE_TP(int_64bit, int)
MK_BASE_TP(int64_bits, int64)

MK_BASE_TP(network16_int, int)
MK_BASE_TP(network32_int, int)
MK_BASE_TP(network32_int32, int32)
MK_BASE_TP(network64_int, int)
MK_BASE_TP(network64_int64, int64)

val bin_writer_array_no_length : ('a, 'a array) S1.writer
  [@@deprecated
    "[since 2016-03] this writer was deprecated as it is misleading and unused"]

(** Conversion of binable types *)

val cnv_writer : ('a -> 'b) -> 'b writer -> 'a writer
val cnv_reader : ('b -> 'a) -> 'b reader -> 'a reader
val cnv : (Shape.t -> Shape.t) -> ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

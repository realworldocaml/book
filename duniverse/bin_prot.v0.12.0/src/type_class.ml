(* Tp_class: sizers, writers, and readers in records *)

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

module S1 = struct
  type ('a, 'b) writer = 'a writer0 -> 'b writer0
  type ('a, 'b) reader = 'a reader0 -> 'b reader0
  type ('a, 'b) t = 'a t0 -> 'b t0
end

module S2 = struct
  type ('a, 'b, 'c) writer = 'a writer0 -> ('b, 'c) S1.writer
  type ('a, 'b, 'c) reader = 'a reader0 -> ('b, 'c) S1.reader
  type ('a, 'b, 'c) t = 'a t0 -> ('b, 'c) S1.t
end

module S3 = struct
  type ('a, 'b, 'c, 'd) writer = 'a writer0 -> ('b, 'c, 'd) S2.writer
  type ('a, 'b, 'c, 'd) reader = 'a reader0 -> ('b, 'c, 'd) S2.reader
  type ('a, 'b, 'c, 'd) t = 'a t0 -> ('b, 'c, 'd) S2.t
end

let variant_wrong_type name _buf ~pos_ref _x =
  Common.raise_variant_wrong_type name !pos_ref
;;

#define MK_BASE(NAME) \
  let bin_writer_##NAME = \
    { \
      size = Size.bin_size_##NAME; \
      write = Write.bin_write_##NAME; \
    } \
  let bin_reader_##NAME = \
    { \
      read = Read.bin_read_##NAME; \
      vtag_read = variant_wrong_type #NAME; \
    } \
  let bin_shape_##NAME = Shape.bin_shape_##NAME \
  let bin_##NAME = \
    { \
      shape = bin_shape_##NAME; \
      writer = bin_writer_##NAME; \
      reader = bin_reader_##NAME; \
    }

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
MK_BASE(nat0)

#define MK_BASE1(NAME) \
  let bin_writer_##NAME bin_writer_el = \
    { \
      size = (fun v -> Size.bin_size_##NAME bin_writer_el.size v); \
      write = (fun buf ~pos v -> \
        Write.bin_write_##NAME bin_writer_el.write buf ~pos v); \
    } \
  let bin_reader_##NAME bin_reader_el = \
    { \
      read = (fun buf ~pos_ref -> \
        Read.bin_read_##NAME bin_reader_el.read buf ~pos_ref); \
      vtag_read = variant_wrong_type #NAME; \
    } \
  let bin_shape_##NAME = \
    fun x1 -> Shape.bin_shape_##NAME x1 \
  let bin_##NAME bin_el = \
    { \
      shape = bin_shape_##NAME bin_el.shape; \
      writer = bin_writer_##NAME bin_el.writer; \
      reader = bin_reader_##NAME bin_el.reader; \
    }

#define MK_BASE2(NAME) \
  let bin_writer_##NAME bin_writer_el1 bin_writer_el2 = \
    { \
      size = (fun v -> \
        Size.bin_size_##NAME bin_writer_el1.size bin_writer_el2.size v); \
      write = (fun buf ~pos v -> \
        Write.bin_write_##NAME \
          bin_writer_el1.write bin_writer_el2.write buf ~pos v); \
    } \
  let bin_reader_##NAME bin_reader_el1 bin_reader_el2 = \
    { \
      read = (fun buf ~pos_ref -> \
        Read.bin_read_##NAME \
          bin_reader_el1.read bin_reader_el2.read buf ~pos_ref); \
      vtag_read = variant_wrong_type #NAME; \
    } \
  let bin_shape_##NAME = \
    fun x1 x2 -> Shape.bin_shape_##NAME x1 x2 \
  let bin_##NAME bin_el1 bin_el2 = \
    { \
      shape = bin_shape_##NAME bin_el1.shape bin_el2.shape; \
      writer = bin_writer_##NAME bin_el1.writer bin_el2.writer; \
      reader = bin_reader_##NAME bin_el1.reader bin_el2.reader; \
    }

#define MK_BASE3(NAME) \
  let bin_writer_##NAME bin_writer_el1 bin_writer_el2 bin_writer_el3 = \
    { \
      size = (fun v -> \
        Size.bin_size_##NAME \
          bin_writer_el1.size bin_writer_el2.size bin_writer_el3.size v); \
      write = (fun buf ~pos v -> \
        Write.bin_write_##NAME \
          bin_writer_el1.write bin_writer_el2.write \
          bin_writer_el3.write buf ~pos v); \
    } \
  let bin_reader_##NAME bin_reader_el1 bin_reader_el2 bin_reader_el3 = \
    { \
      read = (fun buf ~pos_ref -> \
        Read.bin_read_##NAME \
          bin_reader_el1.read bin_reader_el2.read \
          bin_reader_el3.read buf ~pos_ref); \
      vtag_read = variant_wrong_type #NAME; \
    } \
  let bin_shape_##NAME = \
    fun x1 x2 x3 -> Shape.bin_shape_##NAME x1 x2 x3 \
  let bin_##NAME bin_el1 bin_el2 bin_el3 = \
    { \
      shape = \
        bin_shape_##NAME bin_el1.shape bin_el2.shape bin_el3.shape; \
      writer = \
        bin_writer_##NAME bin_el1.writer bin_el2.writer bin_el3.writer; \
      reader = \
        bin_reader_##NAME bin_el1.reader bin_el2.reader bin_el3.reader; \
    }

MK_BASE1(ref)
MK_BASE1(lazy)
MK_BASE1(option)

MK_BASE2(pair)

MK_BASE3(triple)

MK_BASE1(list)
MK_BASE1(array)

MK_BASE2(hashtbl)

MK_BASE(float32_vec)
MK_BASE(float64_vec)
MK_BASE(vec)
MK_BASE(float32_mat)
MK_BASE(float64_mat)
MK_BASE(mat)
MK_BASE(bigstring)

type float_array = float array
MK_BASE(float_array)

MK_BASE(variant_int)
MK_BASE(int_8bit)
MK_BASE(int_16bit)
MK_BASE(int_32bit)
MK_BASE(int_64bit)
MK_BASE(int64_bits)

MK_BASE(network16_int)
MK_BASE(network32_int)
MK_BASE(network32_int32)
MK_BASE(network64_int)
MK_BASE(network64_int64)

let bin_writer_array_no_length bin_writer_el =
  { size  = (fun v ->
      (Size.bin_size_array_no_length [@warning "-3"]) bin_writer_el.size v)
  ; write = (fun buf ~pos v ->
      (Write.bin_write_array_no_length [@warning "-3"]) bin_writer_el.write buf ~pos v)
  }

(* Conversion of binable types *)

let cnv_writer cnv tp_class =
  {
    size = (fun v -> tp_class.size (cnv v));
    write = (fun buf ~pos v -> tp_class.write buf ~pos (cnv v));
  }

let cnv_reader cnv tp_class =
  {
    read = (fun buf ~pos_ref -> cnv (tp_class.read buf ~pos_ref));
    vtag_read = (fun buf ~pos_ref vtag ->
      cnv (tp_class.vtag_read buf ~pos_ref vtag));
  }

let cnv for_shape for_writer for_reader tp_class =
  {
    shape = for_shape tp_class.shape;
    writer = cnv_writer for_writer tp_class.writer;
    reader = cnv_reader for_reader tp_class.reader;
  }

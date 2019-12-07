(**
   This module defines default converters for the types defined in the OCaml
   standard library.
*)

include Size

let bin_unit = Type_class.bin_unit
let bin_shape_unit = Type_class.bin_shape_unit
let bin_writer_unit = Type_class.bin_writer_unit
let bin_write_unit = Write.bin_write_unit
let bin_reader_unit = Type_class.bin_reader_unit
let bin_read_unit = Read.bin_read_unit
let __bin_read_unit__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "unit" !pos_ref

let bin_bool = Type_class.bin_bool
let bin_shape_bool = Type_class.bin_shape_bool
let bin_writer_bool = Type_class.bin_writer_bool
let bin_write_bool = Write.bin_write_bool
let bin_reader_bool = Type_class.bin_reader_bool
let bin_read_bool = Read.bin_read_bool
let __bin_read_bool__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "bool" !pos_ref

let bin_string = Type_class.bin_string
let bin_shape_string = Type_class.bin_shape_string
let bin_writer_string = Type_class.bin_writer_string
let bin_write_string = Write.bin_write_string
let bin_reader_string = Type_class.bin_reader_string
let bin_read_string = Read.bin_read_string
let __bin_read_string__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "string" !pos_ref

let bin_bytes = Type_class.bin_bytes
let bin_shape_bytes = Type_class.bin_shape_bytes
let bin_writer_bytes = Type_class.bin_writer_bytes
let bin_write_bytes = Write.bin_write_bytes
let bin_reader_bytes = Type_class.bin_reader_bytes
let bin_read_bytes = Read.bin_read_bytes
let __bin_read_bytes__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "bytes" !pos_ref

let bin_char = Type_class.bin_char
let bin_shape_char = Type_class.bin_shape_char
let bin_writer_char = Type_class.bin_writer_char
let bin_write_char = Write.bin_write_char
let bin_reader_char = Type_class.bin_reader_char
let bin_read_char = Read.bin_read_char
let __bin_read_char__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "char" !pos_ref

let bin_int = Type_class.bin_int
let bin_shape_int = Type_class.bin_shape_int
let bin_writer_int = Type_class.bin_writer_int
let bin_write_int = Write.bin_write_int
let bin_reader_int = Type_class.bin_reader_int
let bin_read_int = Read.bin_read_int
let __bin_read_int__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "int" !pos_ref

let bin_float = Type_class.bin_float
let bin_shape_float = Type_class.bin_shape_float
let bin_writer_float = Type_class.bin_writer_float
let bin_write_float = Write.bin_write_float
let bin_reader_float = Type_class.bin_reader_float
let bin_read_float = Read.bin_read_float
let __bin_read_float__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "float" !pos_ref

type float_array = float array
let bin_float_array = Type_class.bin_float_array
let bin_shape_float_array = Type_class.bin_shape_float_array
let bin_writer_float_array = Type_class.bin_writer_float_array
let bin_write_float_array = Write.bin_write_float_array
let bin_reader_float_array = Type_class.bin_reader_float_array
let bin_read_float_array = Read.bin_read_float_array
let __bin_read_float_array__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "float_array" !pos_ref

let bin_int32 = Type_class.bin_int32
let bin_shape_int32 = Type_class.bin_shape_int32
let bin_writer_int32 = Type_class.bin_writer_int32
let bin_write_int32 = Write.bin_write_int32
let bin_reader_int32 = Type_class.bin_reader_int32
let bin_read_int32 = Read.bin_read_int32
let __bin_read_int32__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "int32" !pos_ref

let bin_int64 = Type_class.bin_int64
let bin_shape_int64 = Type_class.bin_shape_int64
let bin_writer_int64 = Type_class.bin_writer_int64
let bin_write_int64 = Write.bin_write_int64
let bin_reader_int64 = Type_class.bin_reader_int64
let bin_read_int64 = Read.bin_read_int64
let __bin_read_int64__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "int64" !pos_ref

let bin_nativeint = Type_class.bin_nativeint
let bin_shape_nativeint = Type_class.bin_shape_nativeint
let bin_writer_nativeint = Type_class.bin_writer_nativeint
let bin_write_nativeint = Write.bin_write_nativeint
let bin_reader_nativeint = Type_class.bin_reader_nativeint
let bin_read_nativeint = Read.bin_read_nativeint
let __bin_read_nativeint__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "nativeint" !pos_ref

let bin_ref = Type_class.bin_ref
let bin_shape_ref = Type_class.bin_shape_ref
let bin_writer_ref = Type_class.bin_writer_ref
let bin_write_ref = Write.bin_write_ref
let bin_reader_ref = Type_class.bin_reader_ref
let bin_read_ref = Read.bin_read_ref
let __bin_read_ref__ _f _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "ref" !pos_ref

let bin_lazy_t = Type_class.bin_lazy
let bin_shape_lazy_t = Type_class.bin_shape_lazy
let bin_writer_lazy_t = Type_class.bin_writer_lazy
let bin_write_lazy_t = Write.bin_write_lazy
let bin_reader_lazy_t = Type_class.bin_reader_lazy
let bin_read_lazy_t = Read.bin_read_lazy
let __bin_read_lazy_t__ _f _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "lazy" !pos_ref

let bin_lazy = Type_class.bin_lazy
let bin_shape_lazy = Type_class.bin_shape_lazy
let bin_writer_lazy = Type_class.bin_writer_lazy
let bin_write_lazy = Write.bin_write_lazy
let bin_reader_lazy = Type_class.bin_reader_lazy
let bin_read_lazy = Read.bin_read_lazy
let __bin_read_lazy__ _f _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "lazy" !pos_ref

let bin_option = Type_class.bin_option
let bin_shape_option = Type_class.bin_shape_option
let bin_writer_option = Type_class.bin_writer_option
let bin_write_option = Write.bin_write_option
let bin_reader_option = Type_class.bin_reader_option
let bin_read_option = Read.bin_read_option
let __bin_read_option__ _f _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "option" !pos_ref

let bin_list = Type_class.bin_list
let bin_shape_list = Type_class.bin_shape_list
let bin_writer_list = Type_class.bin_writer_list
let bin_write_list = Write.bin_write_list
let bin_reader_list = Type_class.bin_reader_list
let bin_read_list = Read.bin_read_list
let __bin_read_list__ _f _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "list" !pos_ref

let bin_array = Type_class.bin_array
let bin_shape_array = Type_class.bin_shape_array
let bin_writer_array = Type_class.bin_writer_array
let bin_write_array = Write.bin_write_array
let bin_reader_array = Type_class.bin_reader_array
let bin_read_array = Read.bin_read_array
let __bin_read_array__ _f _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "array" !pos_ref

let bin_hashtbl = Type_class.bin_hashtbl
let bin_shape_hashtbl = Type_class.bin_shape_hashtbl
let bin_writer_hashtbl = Type_class.bin_writer_hashtbl
let bin_write_hashtbl = Write.bin_write_hashtbl
let bin_reader_hashtbl = Type_class.bin_reader_hashtbl
let bin_read_hashtbl = Read.bin_read_hashtbl
let __bin_read_hashtbl__ _f _g _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "hashtbl" !pos_ref

let bin_bigstring = Type_class.bin_bigstring
let bin_shape_bigstring = Type_class.bin_shape_bigstring
let bin_writer_bigstring = Type_class.bin_writer_bigstring
let bin_write_bigstring = Write.bin_write_bigstring
let bin_reader_bigstring = Type_class.bin_reader_bigstring
let bin_read_bigstring = Read.bin_read_bigstring
let __bin_read_bigstring__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "bigstring" !pos_ref

let bin_mat = Type_class.bin_mat
let bin_shape_mat = Type_class.bin_shape_mat
let bin_writer_mat = Type_class.bin_writer_mat
let bin_write_mat = Write.bin_write_mat
let bin_reader_mat = Type_class.bin_reader_mat
let bin_read_mat = Read.bin_read_mat
let __bin_read_mat__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "mat" !pos_ref

let bin_float32_mat = Type_class.bin_float32_mat
let bin_shape_float32_mat = Type_class.bin_shape_float32_mat
let bin_writer_float32_mat = Type_class.bin_writer_float32_mat
let bin_write_float32_mat = Write.bin_write_float32_mat
let bin_reader_float32_mat = Type_class.bin_reader_float32_mat
let bin_read_float32_mat = Read.bin_read_float32_mat
let __bin_read_float32_mat__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "float32_mat" !pos_ref

let bin_float64_mat = Type_class.bin_float64_mat
let bin_shape_float64_mat = Type_class.bin_shape_float64_mat
let bin_writer_float64_mat = Type_class.bin_writer_float64_mat
let bin_write_float64_mat = Write.bin_write_float64_mat
let bin_reader_float64_mat = Type_class.bin_reader_float64_mat
let bin_read_float64_mat = Read.bin_read_float64_mat
let __bin_read_float64_mat__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "float64_mat" !pos_ref

let bin_vec = Type_class.bin_vec
let bin_shape_vec = Type_class.bin_shape_vec
let bin_writer_vec = Type_class.bin_writer_vec
let bin_write_vec = Write.bin_write_vec
let bin_reader_vec = Type_class.bin_reader_vec
let bin_read_vec = Read.bin_read_vec
let __bin_read_vec__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "vec" !pos_ref

let bin_float32_vec = Type_class.bin_float32_vec
let bin_shape_float32_vec = Type_class.bin_shape_float32_vec
let bin_writer_float32_vec = Type_class.bin_writer_float32_vec
let bin_write_float32_vec = Write.bin_write_float32_vec
let bin_reader_float32_vec = Type_class.bin_reader_float32_vec
let bin_read_float32_vec = Read.bin_read_float32_vec
let __bin_read_float32_vec__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "float32_vec" !pos_ref

let bin_float64_vec = Type_class.bin_float64_vec
let bin_shape_float64_vec = Type_class.bin_shape_float64_vec
let bin_writer_float64_vec = Type_class.bin_writer_float64_vec
let bin_write_float64_vec = Write.bin_write_float64_vec
let bin_reader_float64_vec = Type_class.bin_reader_float64_vec
let bin_read_float64_vec = Read.bin_read_float64_vec
let __bin_read_float64_vec__ _buf ~pos_ref _vint =
  Common.raise_variant_wrong_type "float64_vec" !pos_ref

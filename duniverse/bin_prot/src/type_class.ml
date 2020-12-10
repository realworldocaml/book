(* Tp_class: sizers, writers, and readers in records *)

type 'a writer =
  { size : 'a Size.sizer
  ; write : 'a Write.writer
  }

type 'a reader =
  { read : 'a Read.reader
  ; vtag_read : (int -> 'a) Read.reader
  }

type 'a t =
  { shape : Shape.t
  ; writer : 'a writer
  ; reader : 'a reader
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

(*$ open Bin_prot_cinaps.Str *)
(*$ mk_base "unit" *)
let bin_writer_unit = { size = Size.bin_size_unit; write = Write.bin_write_unit }

let bin_reader_unit =
  { read = Read.bin_read_unit; vtag_read = variant_wrong_type "unit" }
;;

let bin_shape_unit = Shape.bin_shape_unit

let bin_unit =
  { shape = bin_shape_unit; writer = bin_writer_unit; reader = bin_reader_unit }
;;

(*$ mk_base "bool" *)
let bin_writer_bool = { size = Size.bin_size_bool; write = Write.bin_write_bool }

let bin_reader_bool =
  { read = Read.bin_read_bool; vtag_read = variant_wrong_type "bool" }
;;

let bin_shape_bool = Shape.bin_shape_bool

let bin_bool =
  { shape = bin_shape_bool; writer = bin_writer_bool; reader = bin_reader_bool }
;;

(*$ mk_base "string" *)
let bin_writer_string = { size = Size.bin_size_string; write = Write.bin_write_string }

let bin_reader_string =
  { read = Read.bin_read_string; vtag_read = variant_wrong_type "string" }
;;

let bin_shape_string = Shape.bin_shape_string

let bin_string =
  { shape = bin_shape_string; writer = bin_writer_string; reader = bin_reader_string }
;;

(*$ mk_base "bytes" *)
let bin_writer_bytes = { size = Size.bin_size_bytes; write = Write.bin_write_bytes }

let bin_reader_bytes =
  { read = Read.bin_read_bytes; vtag_read = variant_wrong_type "bytes" }
;;

let bin_shape_bytes = Shape.bin_shape_bytes

let bin_bytes =
  { shape = bin_shape_bytes; writer = bin_writer_bytes; reader = bin_reader_bytes }
;;

(*$ mk_base "char" *)
let bin_writer_char = { size = Size.bin_size_char; write = Write.bin_write_char }

let bin_reader_char =
  { read = Read.bin_read_char; vtag_read = variant_wrong_type "char" }
;;

let bin_shape_char = Shape.bin_shape_char

let bin_char =
  { shape = bin_shape_char; writer = bin_writer_char; reader = bin_reader_char }
;;

(*$ mk_base "int" *)
let bin_writer_int = { size = Size.bin_size_int; write = Write.bin_write_int }
let bin_reader_int = { read = Read.bin_read_int; vtag_read = variant_wrong_type "int" }
let bin_shape_int = Shape.bin_shape_int
let bin_int = { shape = bin_shape_int; writer = bin_writer_int; reader = bin_reader_int }

(*$ mk_base "float" *)
let bin_writer_float = { size = Size.bin_size_float; write = Write.bin_write_float }

let bin_reader_float =
  { read = Read.bin_read_float; vtag_read = variant_wrong_type "float" }
;;

let bin_shape_float = Shape.bin_shape_float

let bin_float =
  { shape = bin_shape_float; writer = bin_writer_float; reader = bin_reader_float }
;;

(*$ mk_base "int32" *)
let bin_writer_int32 = { size = Size.bin_size_int32; write = Write.bin_write_int32 }

let bin_reader_int32 =
  { read = Read.bin_read_int32; vtag_read = variant_wrong_type "int32" }
;;

let bin_shape_int32 = Shape.bin_shape_int32

let bin_int32 =
  { shape = bin_shape_int32; writer = bin_writer_int32; reader = bin_reader_int32 }
;;

(*$ mk_base "int64" *)
let bin_writer_int64 = { size = Size.bin_size_int64; write = Write.bin_write_int64 }

let bin_reader_int64 =
  { read = Read.bin_read_int64; vtag_read = variant_wrong_type "int64" }
;;

let bin_shape_int64 = Shape.bin_shape_int64

let bin_int64 =
  { shape = bin_shape_int64; writer = bin_writer_int64; reader = bin_reader_int64 }
;;

(*$ mk_base "nativeint" *)
let bin_writer_nativeint =
  { size = Size.bin_size_nativeint; write = Write.bin_write_nativeint }
;;

let bin_reader_nativeint =
  { read = Read.bin_read_nativeint; vtag_read = variant_wrong_type "nativeint" }
;;

let bin_shape_nativeint = Shape.bin_shape_nativeint

let bin_nativeint =
  { shape = bin_shape_nativeint
  ; writer = bin_writer_nativeint
  ; reader = bin_reader_nativeint
  }
;;

(*$ mk_base "nat0" *)
let bin_writer_nat0 = { size = Size.bin_size_nat0; write = Write.bin_write_nat0 }

let bin_reader_nat0 =
  { read = Read.bin_read_nat0; vtag_read = variant_wrong_type "nat0" }
;;

let bin_shape_nat0 = Shape.bin_shape_nat0

let bin_nat0 =
  { shape = bin_shape_nat0; writer = bin_writer_nat0; reader = bin_reader_nat0 }
;;

(*$ mk_base1 "ref" *)
let bin_writer_ref bin_writer_el =
  { size = (fun v -> Size.bin_size_ref bin_writer_el.size v)
  ; write = (fun buf ~pos v -> Write.bin_write_ref bin_writer_el.write buf ~pos v)
  }
;;

let bin_reader_ref bin_reader_el =
  { read = (fun buf ~pos_ref -> Read.bin_read_ref bin_reader_el.read buf ~pos_ref)
  ; vtag_read = variant_wrong_type "ref"
  }
;;

let bin_shape_ref x1 = Shape.bin_shape_ref x1

let bin_ref bin_el =
  { shape = bin_shape_ref bin_el.shape
  ; writer = bin_writer_ref bin_el.writer
  ; reader = bin_reader_ref bin_el.reader
  }
;;

(*$ mk_base1 "lazy" *)
let bin_writer_lazy bin_writer_el =
  { size = (fun v -> Size.bin_size_lazy bin_writer_el.size v)
  ; write = (fun buf ~pos v -> Write.bin_write_lazy bin_writer_el.write buf ~pos v)
  }
;;

let bin_reader_lazy bin_reader_el =
  { read = (fun buf ~pos_ref -> Read.bin_read_lazy bin_reader_el.read buf ~pos_ref)
  ; vtag_read = variant_wrong_type "lazy"
  }
;;

let bin_shape_lazy x1 = Shape.bin_shape_lazy x1

let bin_lazy bin_el =
  { shape = bin_shape_lazy bin_el.shape
  ; writer = bin_writer_lazy bin_el.writer
  ; reader = bin_reader_lazy bin_el.reader
  }
;;

(*$ mk_base1 "option" *)
let bin_writer_option bin_writer_el =
  { size = (fun v -> Size.bin_size_option bin_writer_el.size v)
  ; write = (fun buf ~pos v -> Write.bin_write_option bin_writer_el.write buf ~pos v)
  }
;;

let bin_reader_option bin_reader_el =
  { read = (fun buf ~pos_ref -> Read.bin_read_option bin_reader_el.read buf ~pos_ref)
  ; vtag_read = variant_wrong_type "option"
  }
;;

let bin_shape_option x1 = Shape.bin_shape_option x1

let bin_option bin_el =
  { shape = bin_shape_option bin_el.shape
  ; writer = bin_writer_option bin_el.writer
  ; reader = bin_reader_option bin_el.reader
  }
;;

(*$ mk_base2 "pair" *)
let bin_writer_pair bin_writer_el1 bin_writer_el2 =
  { size = (fun v -> Size.bin_size_pair bin_writer_el1.size bin_writer_el2.size v)
  ; write =
      (fun buf ~pos v ->
         Write.bin_write_pair bin_writer_el1.write bin_writer_el2.write buf ~pos v)
  }
;;

let bin_reader_pair bin_reader_el1 bin_reader_el2 =
  { read =
      (fun buf ~pos_ref ->
         Read.bin_read_pair bin_reader_el1.read bin_reader_el2.read buf ~pos_ref)
  ; vtag_read = variant_wrong_type "pair"
  }
;;

let bin_shape_pair x1 x2 = Shape.bin_shape_pair x1 x2

let bin_pair bin_el1 bin_el2 =
  { shape = bin_shape_pair bin_el1.shape bin_el2.shape
  ; writer = bin_writer_pair bin_el1.writer bin_el2.writer
  ; reader = bin_reader_pair bin_el1.reader bin_el2.reader
  }
;;

(*$ mk_base3 "triple" *)
let bin_writer_triple bin_writer_el1 bin_writer_el2 bin_writer_el3 =
  { size =
      (fun v ->
         Size.bin_size_triple
           bin_writer_el1.size
           bin_writer_el2.size
           bin_writer_el3.size
           v)
  ; write =
      (fun buf ~pos v ->
         Write.bin_write_triple
           bin_writer_el1.write
           bin_writer_el2.write
           bin_writer_el3.write
           buf
           ~pos
           v)
  }
;;

let bin_reader_triple bin_reader_el1 bin_reader_el2 bin_reader_el3 =
  { read =
      (fun buf ~pos_ref ->
         Read.bin_read_triple
           bin_reader_el1.read
           bin_reader_el2.read
           bin_reader_el3.read
           buf
           ~pos_ref)
  ; vtag_read = variant_wrong_type "triple"
  }
;;

let bin_shape_triple x1 x2 x3 = Shape.bin_shape_triple x1 x2 x3

let bin_triple bin_el1 bin_el2 bin_el3 =
  { shape = bin_shape_triple bin_el1.shape bin_el2.shape bin_el3.shape
  ; writer = bin_writer_triple bin_el1.writer bin_el2.writer bin_el3.writer
  ; reader = bin_reader_triple bin_el1.reader bin_el2.reader bin_el3.reader
  }
;;

(*$ mk_base1 "list" *)
let bin_writer_list bin_writer_el =
  { size = (fun v -> Size.bin_size_list bin_writer_el.size v)
  ; write = (fun buf ~pos v -> Write.bin_write_list bin_writer_el.write buf ~pos v)
  }
;;

let bin_reader_list bin_reader_el =
  { read = (fun buf ~pos_ref -> Read.bin_read_list bin_reader_el.read buf ~pos_ref)
  ; vtag_read = variant_wrong_type "list"
  }
;;

let bin_shape_list x1 = Shape.bin_shape_list x1

let bin_list bin_el =
  { shape = bin_shape_list bin_el.shape
  ; writer = bin_writer_list bin_el.writer
  ; reader = bin_reader_list bin_el.reader
  }
;;

(*$ mk_base1 "array" *)
let bin_writer_array bin_writer_el =
  { size = (fun v -> Size.bin_size_array bin_writer_el.size v)
  ; write = (fun buf ~pos v -> Write.bin_write_array bin_writer_el.write buf ~pos v)
  }
;;

let bin_reader_array bin_reader_el =
  { read = (fun buf ~pos_ref -> Read.bin_read_array bin_reader_el.read buf ~pos_ref)
  ; vtag_read = variant_wrong_type "array"
  }
;;

let bin_shape_array x1 = Shape.bin_shape_array x1

let bin_array bin_el =
  { shape = bin_shape_array bin_el.shape
  ; writer = bin_writer_array bin_el.writer
  ; reader = bin_reader_array bin_el.reader
  }
;;

(*$ mk_base2 "hashtbl" *)
let bin_writer_hashtbl bin_writer_el1 bin_writer_el2 =
  { size = (fun v -> Size.bin_size_hashtbl bin_writer_el1.size bin_writer_el2.size v)
  ; write =
      (fun buf ~pos v ->
         Write.bin_write_hashtbl bin_writer_el1.write bin_writer_el2.write buf ~pos v)
  }
;;

let bin_reader_hashtbl bin_reader_el1 bin_reader_el2 =
  { read =
      (fun buf ~pos_ref ->
         Read.bin_read_hashtbl bin_reader_el1.read bin_reader_el2.read buf ~pos_ref)
  ; vtag_read = variant_wrong_type "hashtbl"
  }
;;

let bin_shape_hashtbl x1 x2 = Shape.bin_shape_hashtbl x1 x2

let bin_hashtbl bin_el1 bin_el2 =
  { shape = bin_shape_hashtbl bin_el1.shape bin_el2.shape
  ; writer = bin_writer_hashtbl bin_el1.writer bin_el2.writer
  ; reader = bin_reader_hashtbl bin_el1.reader bin_el2.reader
  }
;;

(*$ mk_base "float32_vec" *)
let bin_writer_float32_vec =
  { size = Size.bin_size_float32_vec; write = Write.bin_write_float32_vec }
;;

let bin_reader_float32_vec =
  { read = Read.bin_read_float32_vec; vtag_read = variant_wrong_type "float32_vec" }
;;

let bin_shape_float32_vec = Shape.bin_shape_float32_vec

let bin_float32_vec =
  { shape = bin_shape_float32_vec
  ; writer = bin_writer_float32_vec
  ; reader = bin_reader_float32_vec
  }
;;

(*$ mk_base "float64_vec" *)
let bin_writer_float64_vec =
  { size = Size.bin_size_float64_vec; write = Write.bin_write_float64_vec }
;;

let bin_reader_float64_vec =
  { read = Read.bin_read_float64_vec; vtag_read = variant_wrong_type "float64_vec" }
;;

let bin_shape_float64_vec = Shape.bin_shape_float64_vec

let bin_float64_vec =
  { shape = bin_shape_float64_vec
  ; writer = bin_writer_float64_vec
  ; reader = bin_reader_float64_vec
  }
;;

(*$ mk_base "vec" *)
let bin_writer_vec = { size = Size.bin_size_vec; write = Write.bin_write_vec }
let bin_reader_vec = { read = Read.bin_read_vec; vtag_read = variant_wrong_type "vec" }
let bin_shape_vec = Shape.bin_shape_vec
let bin_vec = { shape = bin_shape_vec; writer = bin_writer_vec; reader = bin_reader_vec }

(*$ mk_base "float32_mat" *)
let bin_writer_float32_mat =
  { size = Size.bin_size_float32_mat; write = Write.bin_write_float32_mat }
;;

let bin_reader_float32_mat =
  { read = Read.bin_read_float32_mat; vtag_read = variant_wrong_type "float32_mat" }
;;

let bin_shape_float32_mat = Shape.bin_shape_float32_mat

let bin_float32_mat =
  { shape = bin_shape_float32_mat
  ; writer = bin_writer_float32_mat
  ; reader = bin_reader_float32_mat
  }
;;

(*$ mk_base "float64_mat" *)
let bin_writer_float64_mat =
  { size = Size.bin_size_float64_mat; write = Write.bin_write_float64_mat }
;;

let bin_reader_float64_mat =
  { read = Read.bin_read_float64_mat; vtag_read = variant_wrong_type "float64_mat" }
;;

let bin_shape_float64_mat = Shape.bin_shape_float64_mat

let bin_float64_mat =
  { shape = bin_shape_float64_mat
  ; writer = bin_writer_float64_mat
  ; reader = bin_reader_float64_mat
  }
;;

(*$ mk_base "mat" *)
let bin_writer_mat = { size = Size.bin_size_mat; write = Write.bin_write_mat }
let bin_reader_mat = { read = Read.bin_read_mat; vtag_read = variant_wrong_type "mat" }
let bin_shape_mat = Shape.bin_shape_mat
let bin_mat = { shape = bin_shape_mat; writer = bin_writer_mat; reader = bin_reader_mat }

(*$ mk_base "bigstring" *)
let bin_writer_bigstring =
  { size = Size.bin_size_bigstring; write = Write.bin_write_bigstring }
;;

let bin_reader_bigstring =
  { read = Read.bin_read_bigstring; vtag_read = variant_wrong_type "bigstring" }
;;

let bin_shape_bigstring = Shape.bin_shape_bigstring

let bin_bigstring =
  { shape = bin_shape_bigstring
  ; writer = bin_writer_bigstring
  ; reader = bin_reader_bigstring
  }
;;

(*$*)
type float_array = float array

(*$ mk_base "float_array" *)
let bin_writer_float_array =
  { size = Size.bin_size_float_array; write = Write.bin_write_float_array }
;;

let bin_reader_float_array =
  { read = Read.bin_read_float_array; vtag_read = variant_wrong_type "float_array" }
;;

let bin_shape_float_array = Shape.bin_shape_float_array

let bin_float_array =
  { shape = bin_shape_float_array
  ; writer = bin_writer_float_array
  ; reader = bin_reader_float_array
  }
;;

(*$ mk_base "variant_int" *)
let bin_writer_variant_int =
  { size = Size.bin_size_variant_int; write = Write.bin_write_variant_int }
;;

let bin_reader_variant_int =
  { read = Read.bin_read_variant_int; vtag_read = variant_wrong_type "variant_int" }
;;

let bin_shape_variant_int = Shape.bin_shape_variant_int

let bin_variant_int =
  { shape = bin_shape_variant_int
  ; writer = bin_writer_variant_int
  ; reader = bin_reader_variant_int
  }
;;

(*$ mk_base "int_8bit" *)
let bin_writer_int_8bit =
  { size = Size.bin_size_int_8bit; write = Write.bin_write_int_8bit }
;;

let bin_reader_int_8bit =
  { read = Read.bin_read_int_8bit; vtag_read = variant_wrong_type "int_8bit" }
;;

let bin_shape_int_8bit = Shape.bin_shape_int_8bit

let bin_int_8bit =
  { shape = bin_shape_int_8bit
  ; writer = bin_writer_int_8bit
  ; reader = bin_reader_int_8bit
  }
;;

(*$ mk_base "int_16bit" *)
let bin_writer_int_16bit =
  { size = Size.bin_size_int_16bit; write = Write.bin_write_int_16bit }
;;

let bin_reader_int_16bit =
  { read = Read.bin_read_int_16bit; vtag_read = variant_wrong_type "int_16bit" }
;;

let bin_shape_int_16bit = Shape.bin_shape_int_16bit

let bin_int_16bit =
  { shape = bin_shape_int_16bit
  ; writer = bin_writer_int_16bit
  ; reader = bin_reader_int_16bit
  }
;;

(*$ mk_base "int_32bit" *)
let bin_writer_int_32bit =
  { size = Size.bin_size_int_32bit; write = Write.bin_write_int_32bit }
;;

let bin_reader_int_32bit =
  { read = Read.bin_read_int_32bit; vtag_read = variant_wrong_type "int_32bit" }
;;

let bin_shape_int_32bit = Shape.bin_shape_int_32bit

let bin_int_32bit =
  { shape = bin_shape_int_32bit
  ; writer = bin_writer_int_32bit
  ; reader = bin_reader_int_32bit
  }
;;

(*$ mk_base "int_64bit" *)
let bin_writer_int_64bit =
  { size = Size.bin_size_int_64bit; write = Write.bin_write_int_64bit }
;;

let bin_reader_int_64bit =
  { read = Read.bin_read_int_64bit; vtag_read = variant_wrong_type "int_64bit" }
;;

let bin_shape_int_64bit = Shape.bin_shape_int_64bit

let bin_int_64bit =
  { shape = bin_shape_int_64bit
  ; writer = bin_writer_int_64bit
  ; reader = bin_reader_int_64bit
  }
;;

(*$ mk_base "int64_bits" *)
let bin_writer_int64_bits =
  { size = Size.bin_size_int64_bits; write = Write.bin_write_int64_bits }
;;

let bin_reader_int64_bits =
  { read = Read.bin_read_int64_bits; vtag_read = variant_wrong_type "int64_bits" }
;;

let bin_shape_int64_bits = Shape.bin_shape_int64_bits

let bin_int64_bits =
  { shape = bin_shape_int64_bits
  ; writer = bin_writer_int64_bits
  ; reader = bin_reader_int64_bits
  }
;;

(*$ mk_base "network16_int" *)
let bin_writer_network16_int =
  { size = Size.bin_size_network16_int; write = Write.bin_write_network16_int }
;;

let bin_reader_network16_int =
  { read = Read.bin_read_network16_int; vtag_read = variant_wrong_type "network16_int" }
;;

let bin_shape_network16_int = Shape.bin_shape_network16_int

let bin_network16_int =
  { shape = bin_shape_network16_int
  ; writer = bin_writer_network16_int
  ; reader = bin_reader_network16_int
  }
;;

(*$ mk_base "network32_int" *)
let bin_writer_network32_int =
  { size = Size.bin_size_network32_int; write = Write.bin_write_network32_int }
;;

let bin_reader_network32_int =
  { read = Read.bin_read_network32_int; vtag_read = variant_wrong_type "network32_int" }
;;

let bin_shape_network32_int = Shape.bin_shape_network32_int

let bin_network32_int =
  { shape = bin_shape_network32_int
  ; writer = bin_writer_network32_int
  ; reader = bin_reader_network32_int
  }
;;

(*$ mk_base "network32_int32" *)
let bin_writer_network32_int32 =
  { size = Size.bin_size_network32_int32; write = Write.bin_write_network32_int32 }
;;

let bin_reader_network32_int32 =
  { read = Read.bin_read_network32_int32
  ; vtag_read = variant_wrong_type "network32_int32"
  }
;;

let bin_shape_network32_int32 = Shape.bin_shape_network32_int32

let bin_network32_int32 =
  { shape = bin_shape_network32_int32
  ; writer = bin_writer_network32_int32
  ; reader = bin_reader_network32_int32
  }
;;

(*$ mk_base "network64_int" *)
let bin_writer_network64_int =
  { size = Size.bin_size_network64_int; write = Write.bin_write_network64_int }
;;

let bin_reader_network64_int =
  { read = Read.bin_read_network64_int; vtag_read = variant_wrong_type "network64_int" }
;;

let bin_shape_network64_int = Shape.bin_shape_network64_int

let bin_network64_int =
  { shape = bin_shape_network64_int
  ; writer = bin_writer_network64_int
  ; reader = bin_reader_network64_int
  }
;;

(*$ mk_base "network64_int64" *)
let bin_writer_network64_int64 =
  { size = Size.bin_size_network64_int64; write = Write.bin_write_network64_int64 }
;;

let bin_reader_network64_int64 =
  { read = Read.bin_read_network64_int64
  ; vtag_read = variant_wrong_type "network64_int64"
  }
;;

let bin_shape_network64_int64 = Shape.bin_shape_network64_int64

let bin_network64_int64 =
  { shape = bin_shape_network64_int64
  ; writer = bin_writer_network64_int64
  ; reader = bin_reader_network64_int64
  }
;;

(*$*)
let bin_writer_array_no_length bin_writer_el =
  { size =
      (fun v -> (Size.bin_size_array_no_length [@warning "-3"]) bin_writer_el.size v)
  ; write =
      (fun buf ~pos v ->
         (Write.bin_write_array_no_length [@warning "-3"]) bin_writer_el.write buf ~pos v)
  }
;;

(* Conversion of binable types *)

let cnv_writer cnv tp_class =
  { size = (fun v -> tp_class.size (cnv v))
  ; write = (fun buf ~pos v -> tp_class.write buf ~pos (cnv v))
  }
;;

let cnv_reader cnv tp_class =
  { read = (fun buf ~pos_ref -> cnv (tp_class.read buf ~pos_ref))
  ; vtag_read = (fun buf ~pos_ref vtag -> cnv (tp_class.vtag_read buf ~pos_ref vtag))
  }
;;

let cnv for_shape for_writer for_reader tp_class =
  { shape = for_shape tp_class.shape
  ; writer = cnv_writer for_writer tp_class.writer
  ; reader = cnv_reader for_reader tp_class.reader
  }
;;

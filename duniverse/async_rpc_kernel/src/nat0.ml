type t = Bin_prot.Nat0.t [@@deriving bin_shape ~basetype:"899e2f4a-490a-11e6-b68f-bbd62472516c"]

let bin_t = Bin_prot.Type_class.bin_nat0
let bin_size_t = Bin_prot.Size.bin_size_nat0
let bin_writer_t = Bin_prot.Type_class.bin_writer_nat0
let bin_write_t = Bin_prot.Write.bin_write_nat0
let bin_reader_t = Bin_prot.Type_class.bin_reader_nat0
let bin_read_t = Bin_prot.Read.bin_read_nat0
let __bin_read_t__ _buf ~pos_ref _vnat0 =
  Bin_prot.Common.raise_variant_wrong_type "t" !pos_ref

let of_int_exn  = Bin_prot.Nat0.of_int

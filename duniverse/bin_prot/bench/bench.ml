open Bin_prot
open Core

let%bench_module "float array" = (module struct
  let a = Array.create ~len:1000 0.
  let buf =
    let buf = Common.create_buf (1000 * 8 + 8) in
    let _ = Write.bin_write_float_array buf ~pos:0 a in
    buf

  module Price = struct
    type t = float [@@deriving bin_io]
  end
  let price_array : Price.t array = Array.create ~len:1000 0.

  let size_float f = Size.bin_size_float f
  let%bench "size    non optimal" = Size.bin_size_array size_float a
  let%bench "size    float array" = Size.bin_size_float_array a
  let%bench "size  Price.t array" = Size.bin_size_array Price.bin_size_t price_array

  let write_float buf ~pos f = Write.bin_write_float buf ~pos f
  let%bench "write   non optimal" =
    let _ = Write.bin_write_array write_float buf ~pos:0 a in ()
  let%bench "write   float array" =
    let _ = Write.bin_write_float_array buf ~pos:0 a in ()
  let%bench "write Price.t array" =
    let _ = Write.bin_write_array Price.bin_write_t buf ~pos:0 a in ()

  let read_float buf ~pos_ref = Read.bin_read_float buf ~pos_ref
  let%bench "read    non optimal" =
    let pos_ref = ref 0 in
    let _ = Read.bin_read_array read_float buf ~pos_ref in ()
  let%bench "read    float array" =
    let pos_ref = ref 0 in
    let _ = Read.bin_read_float_array buf ~pos_ref in ()
  let%bench "read  Price.t array" =
    let pos_ref = ref 0 in
    let _ = Read.bin_read_array Price.bin_read_t buf ~pos_ref in ()

  let int_array = Array.create ~len:1000 0

  let%bench "int array  size" = Size.bin_size_array Size.bin_size_int int_array
  let%bench "int array write" =
    let _ = Write.bin_write_array Write.bin_write_int buf ~pos:0 int_array in ()
  let%bench "int array  read" =
    let pos_ref = ref 0 in
    let _ = Read.bin_read_array Read.bin_read_int buf ~pos_ref in ()

  module Book = struct
    type t = {
      a : Price.t array;
    } [@@deriving bin_io]
  end

  let book = { Book.a = Array.create ~len:1000 0. }
  let buf =
    let buf = Common.create_buf (2100 * 8) in
    let _ = Book.bin_write_t buf ~pos:0 book in
    buf

  let%bench "size  field" = Book.bin_size_t book
  let%bench "write field" = Book.bin_write_t buf ~pos:0 book
  let%bench "read  field" =
    let pos_ref = ref 0 in
    let _ = Book.bin_read_t buf ~pos_ref in ()
end)

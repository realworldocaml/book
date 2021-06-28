(* WARNING: never accept the corrected output for this file, it must never change! *)

open! Core_kernel
open! Import
module Blob = Bin_prot.Blob
module Opaque = Blob.Opaque
module Read = Bin_prot.Read
module Shape = Bin_prot.Shape
module Type_class = Bin_prot.Type_class
module Write = Bin_prot.Write

let buf = lazy (Bin_prot.Common.create_buf 10_000)

let bin_dump_to_string bin_a a =
  let buf = Bin_prot.Utils.bin_dump bin_a.Type_class.writer a in
  let len = Bin_prot.Common.buf_len buf in
  let str = Bytes.create len in
  Bin_prot.Common.blit_buf_string buf str ~len;
  Bytes.to_string str
;;

let bin_read_from_string bin_reader str =
  let buf = Lazy.force buf in
  let len = String.length str in
  Bin_prot.Common.blit_string_buf str buf ~len;
  let pos_ref = ref 0 in
  let a = bin_reader.Type_class.read buf ~pos_ref in
  assert (Int.( = ) !pos_ref len);
  a
;;

let print_bin_dump tag bin_a a =
  printf "%s : %s\n" tag (bin_dump_to_string bin_a a |> String.escaped)
;;

let run_test (bin_a : 'a Type_class.t) equal a : unit =
  print_bin_dump "bin dump" bin_a a;
  print_bin_dump "bin dump (blob)" (Blob.bin_t bin_a) a;
  let buf = Lazy.force buf in
  let dump_and_read bin_t equal t =
    let str = bin_dump_to_string bin_t t in
    assert (equal t (bin_read_from_string bin_t.reader str));
    let (_ : Blob.Ignored.t) = bin_read_from_string Blob.Ignored.bin_reader_t str in
    str
  in
  let bin_dump_blob = dump_and_read (Blob.bin_t bin_a) equal a in
  let bin_dump_opaque_big_string =
    dump_and_read
      Opaque.Bigstring.bin_t
      Poly.equal
      (Opaque.Bigstring.to_opaque a bin_a.writer)
  in
  let bin_dump_opaque_string =
    dump_and_read
      Opaque.String.bin_t
      Poly.equal
      (Opaque.String.to_opaque ~buf a bin_a.writer)
  in
  printf
    !"bin dump as opaque big string and string are the same? %{Bool}\n"
    (String.( = ) bin_dump_blob bin_dump_opaque_big_string
     && String.( = ) bin_dump_blob bin_dump_opaque_string)
;;

let%expect_test "" =
  run_test Int.Stable.V1.bin_t Int.equal 12;
  [%expect
    {|
    bin dump : \012
    bin dump (blob) : \001\000\000\000\000\000\000\000\012
    bin dump as opaque big string and string are the same? true
  |}];
  run_test String.Stable.V1.bin_t String.equal "Testing string";
  [%expect
    {|
    bin dump : \014Testing string
    bin dump (blob) : \015\000\000\000\000\000\000\000\014Testing string
    bin dump as opaque big string and string are the same? true
  |}];
  run_test Float_with_finite_only_serialization.Stable.V1.bin_t Float.equal 1.234;
  [%expect
    {|
    bin dump : X9\180\200v\190\243?
    bin dump (blob) : \b\000\000\000\000\000\000\000X9\180\200v\190\243?
    bin dump as opaque big string and string are the same? true
  |}];
  run_test
    Host_and_port.Stable.V1.bin_t
    Host_and_port.equal
    (Host_and_port.create ~host:"testhost" ~port:123);
  [%expect
    {|
    bin dump : \btesthost{
    bin dump (blob) : \n\000\000\000\000\000\000\000\btesthost{
    bin dump as opaque big string and string are the same? true
  |}];
  run_test (bin_list Int.Stable.V1.bin_t) (List.equal Int.equal) [ 1; 3; 1; 5 ];
  [%expect
    {|
    bin dump : \004\001\003\001\005
    bin dump (blob) : \005\000\000\000\000\000\000\000\004\001\003\001\005
    bin dump as opaque big string and string are the same? true
  |}]
;;

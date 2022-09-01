open! Core
open! Import

let run_test = Blob_tests.run_stability_test

(* WARNING: never accept the corrected output for this test, it must never change! *)
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

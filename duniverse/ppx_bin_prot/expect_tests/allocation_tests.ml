open! Core_kernel
open! Expect_test_helpers_kernel

let%expect_test "No allocation when writing variants" =
  let module Test = struct
    type t =
      | No_arg
      | One_arg of string
      | Record_arg of { num : int }
    [@@deriving bin_io]
  end in
  let writer = Test.bin_writer_t in
  let buf = Bigstring.create 1024 in
  let _ : int =
    require_no_allocation [%here] (fun () -> writer.write buf ~pos:0 No_arg) in
  let one_arg = Test.One_arg "foo" in
  let () = [%expect {| |}] in
  let _ : int =
    require_no_allocation [%here] (fun () -> writer.write buf ~pos:0 one_arg) in
  let () = [%expect {| |}] in
  let record_arg = Test.Record_arg { num = 5 } in
  let _ : int =
    require_no_allocation [%here] (fun () -> writer.write buf ~pos:0 record_arg) in
  [%expect {| |}];
;;

let%expect_test "No allocation when writing polymorphic variant" =
  let module Test = struct
    type t =
      [ `No_arg
      | `One_arg of string ]
    [@@deriving bin_io]
  end in
  let writer = Test.bin_writer_t in
  let buf = Bigstring.create 1024 in
  let _ : int =
    require_no_allocation [%here] (fun () -> writer.write buf ~pos:0 `No_arg) in
  let one_arg = `One_arg "foo" in
  let () = [%expect {| |}] in
  let _ : int =
    require_no_allocation [%here] (fun () -> writer.write buf ~pos:0 one_arg) in
  [%expect {| |}]
;;

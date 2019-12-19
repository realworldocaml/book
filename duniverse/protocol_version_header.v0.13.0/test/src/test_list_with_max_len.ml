open! Core_kernel

module List_with_max_len_one =
  Protocol_version_header.For_test.Make_list_with_max_len (struct
    let max_len = 1
    let context = Info.of_string "Context"
  end)

let round_trips ~here lst =
  let serialized_with_regular_list =
    Bin_prot.Utils.bin_dump ~header:false (List.bin_writer_t Int.bin_writer_t) lst
  in
  let final_list =
    List_with_max_len_one.bin_read_t
      Int.bin_read_t
      serialized_with_regular_list
      ~pos_ref:(ref 0)
  in
  [%test_eq: int list] ~here:[ here ] lst (final_list :> int list)
;;

let%expect_test "we can read regular lists sent over the wire and fail when we are \
                 supposed to. The behavior is necessary in case someone sends garbage \
                 on the wire."
  =
  round_trips ~here:[%here] [];
  round_trips ~here:[%here] [ 1 ];
  try round_trips ~here:[%here] [ 1; 2 ] with
  | exn ->
    print_s [%message (exn : Exn.t)];
    [%expect
      {|
          (exn
           (exn.ml.Reraised Context
            (common.ml.Read_error "List_too_long / 2 (max 1)" 1)))|}]
;;

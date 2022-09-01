open! Core
open! Import

let require_no_allocation here f =
  let ( result
      , ({ major_words_allocated; minor_words_allocated } :
           Gc.For_testing.Allocation_report.t) )
    =
    Gc.For_testing.measure_allocation f
  in
  if major_words_allocated > 0 || minor_words_allocated > 0
  then
    raise_s
      [%sexp
        "Unexpected allocations"
      , (here : Source_code_position.t)
      , { major_words_allocated : int; minor_words_allocated : int }];
  result
;;

let%expect_test "" =
  let buf = Bin_prot.Common.create_buf 10_000 in
  let run_test here (bin_t : _ Bin_prot.Type_class.t) l =
    let size =
      require_no_allocation here (fun () ->
        Bin_prot.Size.bin_size_list bin_t.writer.size l)
    in
    printf "size: %d\n" size;
    let pos =
      require_no_allocation here (fun () ->
        Bin_prot.Write.bin_write_list bin_t.writer.write buf ~pos:0 l)
    in
    printf "pos after writing: %d\n" pos;
    ()
  in
  run_test [%here] Int.Stable.V1.bin_t [ 1; 2; 3; 4; 5 ];
  [%expect {|
    size: 6
    pos after writing: 6 |}];
  run_test [%here] String.Stable.V1.bin_t [ "one"; "two"; "three"; "four" ];
  [%expect {|
    size: 20
    pos after writing: 20 |}]
;;

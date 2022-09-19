open! Base
open Expect_test_helpers_core

let%expect_test _ =
  let x = Sys.opaque_identity "one string" in
  let y = Sys.opaque_identity "another" in
  require_no_allocation [%here] (fun () ->
    ignore (Sys.opaque_identity (String.Caseless.equal x y) : bool));
  [%expect {||}]
;;

let%expect_test "empty substring" =
  let string = String.init 10 ~f:Char.of_int_exn in
  let test here f =
    let substring = require_no_allocation here f in
    assert (String.is_empty substring)
  in
  test [%here] (fun () -> String.sub string ~pos:0 ~len:0);
  test [%here] (fun () -> String.prefix string 0);
  test [%here] (fun () -> String.suffix string 0);
  test [%here] (fun () -> String.drop_prefix string 10);
  test [%here] (fun () -> String.drop_suffix string 10);
  [%expect {| |}]
;;

let%expect_test "mem does not allocate" =
  let string = Sys.opaque_identity "abracadabra" in
  let char = Sys.opaque_identity 'd' in
  require_no_allocation [%here] (fun () -> ignore (String.mem string char : bool));
  [%expect {||}]
;;

let%expect_test "fold does not allocate" =
  let string = Sys.opaque_identity "abracadabra" in
  let char = Sys.opaque_identity 'd' in
  let f acc c = if Char.equal c char then true else acc in
  require_no_allocation [%here] (fun () ->
    ignore (String.fold string ~init:false ~f : bool));
  [%expect {||}]
;;

let%expect_test "foldi does not allocate" =
  let string = Sys.opaque_identity "abracadabra" in
  let char = Sys.opaque_identity 'd' in
  let f _i acc c = if Char.equal c char then true else acc in
  require_no_allocation [%here] (fun () ->
    ignore (String.foldi string ~init:false ~f : bool));
  [%expect {||}]
;;

let%test_module "common prefix and suffix" =
  (module struct
    let require_int_equal a b ~message = require_equal [%here] (module Int) a b ~message

    let require_string_equal a b ~message =
      require_equal [%here] (module String) a b ~message
    ;;

    let simulate_common_length ~get_common2_length list =
      let rec loop acc prev list ~get_common2_length =
        match list with
        | [] -> acc
        | head :: tail ->
          loop (Int.min acc (get_common2_length prev head)) head tail ~get_common2_length
      in
      match list with
      | [] -> 0
      | [ head ] -> String.length head
      | head :: tail -> loop Int.max_value head tail ~get_common2_length
    ;;

    let get_shortest_and_longest list =
      let compare_by_length = Comparable.lift Int.compare ~f:String.length in
      Option.both
        (List.min_elt list ~compare:compare_by_length)
        (List.max_elt list ~compare:compare_by_length)
    ;;

    let test_generic get_common get_common2 get_common_length get_common2_length =
      Staged.stage (fun list ->
        let common = get_common list in
        print_s [%sexp (common : string)];
        let len = get_common_length list in
        require_int_equal len (String.length common) ~message:"wrong length";
        let common2 = List.reduce list ~f:get_common2 |> Option.value ~default:"" in
        require_string_equal common common2 ~message:"pairwise result mismatch";
        let len2 = simulate_common_length ~get_common2_length list in
        require_int_equal len len2 ~message:"pairwise length mismatch";
        if not (String.is_empty common || List.mem list common ~equal:String.equal)
        then print_endline "(may allocate)"
        else (
          ignore (require_no_allocation [%here] (fun () -> get_common list) : string);
          Option.iter (get_shortest_and_longest list) ~f:(fun (shortest, longest) ->
            ignore
              (require_no_allocation [%here] (fun () -> get_common2 shortest longest)
               : string);
            ignore
              (require_no_allocation [%here] (fun () -> get_common2 longest shortest)
               : string))))
    ;;

    let test_prefix =
      test_generic
        String.common_prefix
        String.common_prefix2
        String.common_prefix_length
        String.common_prefix2_length
      |> Staged.unstage
    ;;

    let test_suffix =
      test_generic
        String.common_suffix
        String.common_suffix2
        String.common_suffix_length
        String.common_suffix2_length
      |> Staged.unstage
    ;;

    let%expect_test "empty" =
      test_prefix [];
      [%expect {| "" |}];
      test_suffix [];
      [%expect {| "" |}]
    ;;

    let%expect_test "singleton" =
      test_prefix [ "abut" ];
      [%expect {| abut |}];
      test_suffix [ "tuba" ];
      [%expect {| tuba |}]
    ;;

    let%expect_test "doubleton, alloc" =
      test_prefix [ "hello"; "help"; "hex" ];
      [%expect {|
        he
        (may allocate) |}];
      test_suffix [ "crest"; "zest"; "1st" ];
      [%expect {|
        st
        (may allocate) |}]
    ;;

    let%expect_test "doubleton, no alloc" =
      test_prefix [ "hello"; "help"; "he" ];
      [%expect {| he |}];
      test_suffix [ "crest"; "zest"; "st" ];
      [%expect {| st |}]
    ;;

    let%expect_test "many, alloc" =
      test_prefix [ "this"; "that"; "the other"; "these"; "those"; "thy"; "thou" ];
      [%expect {|
        th
        (may allocate) |}];
      test_suffix [ "fourth"; "fifth"; "sixth"; "seventh"; "eleventh"; "twelfth" ];
      [%expect {|
        th
        (may allocate) |}]
    ;;

    let%expect_test "many, no alloc" =
      test_prefix [ "inconsequential"; "invariant"; "in"; "inner"; "increment" ];
      [%expect {| in |}];
      test_suffix [ "fat"; "cat"; "sat"; "at"; "bat" ];
      [%expect {| at |}]
    ;;

    let%expect_test "many, nothing in common" =
      let lorem_ipsum = [ "lorem"; "ipsum"; "dolor"; "sit"; "amet" ] in
      test_prefix lorem_ipsum;
      [%expect {| "" |}];
      test_suffix lorem_ipsum;
      [%expect {| "" |}]
    ;;
  end)
;;

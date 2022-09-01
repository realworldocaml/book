open! Import

(* These methods miss part of the backtrace. *)

let clobber_most_recent_backtrace () =
  try failwith "clobbering" with
  | _ -> ()
;;

let _Base_Exn_reraise exn = Exn.reraise exn "reraised"

let _Base_Exn_reraise_after_clobbering_most_recent_backtrace exn =
  clobber_most_recent_backtrace ();
  Exn.reraise exn "reraised"
;;

external reraiser_raw : exn -> 'a = "%reraise"

let external_reraise_unequal exn = reraiser_raw (Exn.Reraised ("reraised", exn))
let vanilla_raise_unequal exn = raise (Exn.Reraised ("reraised", exn))

(* These methods produce the full, desired backtrace. *)

let vanilla_raise exn = raise exn

let raise_with_original_backtrace exn =
  let backtrace = Backtrace.Exn.most_recent () in
  Exn.raise_with_original_backtrace (Exn.Reraised ("reraised", exn)) backtrace
;;

(* This ref causes [check_value] to appear in the backtrace, because the [raise_s] call is
   no longer in tail position. *)
let setter = ref 0

let check_value x =
  if x < 0 then raise_s [%message "bad value" (x : int)];
  setter := x
;;

(* This function duplicates the functionality of [Exn.reraise_uncaught] with a custom
   [reraiser] *)
let reraise_uncaught reraiser f =
  try f () with
  | exn -> reraiser exn
;;

let callstacker ~reraise_uncaught =
  let rec loop reraise_uncaught x =
    reraise_uncaught (fun () -> check_value x);
    loop reraise_uncaught (x - 1);
    reraise_uncaught (fun () -> check_value x)
  in
  loop reraise_uncaught 1
;;

let with_backtraces_enabled f =
  Backtrace.Exn.with_recording true ~f:(fun () ->
    Ref.set_temporarily Backtrace.elide false ~f)
;;

let test_reraise_uncaught ~reraise_uncaught =
  with_backtraces_enabled (fun () ->
    Exn.handle_uncaught ~exit:false (fun () -> callstacker ~reraise_uncaught))
;;

let test_reraiser reraiser =
  test_reraise_uncaught ~reraise_uncaught:(reraise_uncaught reraiser)
;;

(* If you want to see what the underlying backtraces look like, set this to true.
   Otherwise, these tests extract small snippets from the backtraces so that they are
   robust to compiler changes. *)
let just_print = false

let really_show_backtrace s =
  if just_print
  then print_endline s
  else
    printf
      "Before re-raise: %b\nAfter  re-raise: %b"
      (String.is_substring s ~substring:"check_value")
      (String.is_substring s ~substring:"handle_uncaught")
;;

let%test_module ("Show native backtraces" [@tags "no-js"]) =
  (module struct
    (* good *)
    let%expect_test "Base.Exn.reraise" =
      test_reraiser _Base_Exn_reraise;
      really_show_backtrace [%expect.output];
      [%expect {|
          Before re-raise: true
          After  re-raise: true |}]
    ;;

    (* bad, because the backtrace was clobbered *)
    let%expect_test "Base.Exn.reraise" =
      test_reraiser _Base_Exn_reraise_after_clobbering_most_recent_backtrace;
      really_show_backtrace [%expect.output];
      [%expect {|
          Before re-raise: false
          After  re-raise: true |}]
    ;;

    (* bad, missing the backtrace before the reraise *)
    let%expect_test "%reraise unequal" =
      test_reraiser external_reraise_unequal;
      really_show_backtrace [%expect.output];
      [%expect {|
          Before re-raise: false
          After  re-raise: true |}]
    ;;

    (* bad, missing the backtrace before the reraise *)
    let%expect_test "raise unequal" =
      test_reraiser vanilla_raise_unequal;
      really_show_backtrace [%expect.output];
      [%expect {|
          Before re-raise: false
          After  re-raise: true |}]
    ;;

    (* good, but no additional info attached *)
    let%expect_test "raise equal" =
      test_reraiser vanilla_raise;
      really_show_backtrace [%expect.output];
      [%expect {|
          Before re-raise: true
          After  re-raise: true |}]
    ;;

    (* good *)
    let%expect_test "Caml.Printexc.raise_with_backtrace" =
      test_reraiser raise_with_original_backtrace;
      really_show_backtrace [%expect.output];
      [%expect {|
          Before re-raise: true
          After  re-raise: true |}]
    ;;

    (* good *)
    let%expect_test "Exn.reraise_uncaught" =
      test_reraise_uncaught ~reraise_uncaught:(Exn.reraise_uncaught "reraised");
      really_show_backtrace [%expect.output];
      [%expect {|
          Before re-raise: true
          After  re-raise: true |}]
    ;;
  end)
;;

(* An example bad backtrace:
   {v
      Uncaught exception:

        (exn.ml.Reraised reraised ("bad value" (x -1)))

      Raised at Base_test__Test_exn_reraise.vanilla_raise_unequal in file "test_exn_reraise.ml" (inlined), line 10, characters 32-70
      Called from Base_test__Test_exn_reraise.reraise_uncaught in file "test_exn_reraise.ml" (inlined), line 34, characters 11-23
      Called from Base_test__Test_exn_reraise.callstacker.loop in file "test_exn_reraise.ml", line 39, characters 4-55
      Called from Base_test__Test_exn_reraise.callstacker.loop in file "test_exn_reraise.ml" (inlined), line 38, characters 15-167
      Called from Base_test__Test_exn_reraise.callstacker.loop in file "test_exn_reraise.ml" (inlined), line 40, characters 4-25
      Called from Base_test__Test_exn_reraise.callstacker.loop in file "test_exn_reraise.ml" (inlined), line 38, characters 15-167
      Called from Base_test__Test_exn_reraise.callstacker.loop in file "test_exn_reraise.ml" (inlined), line 40, characters 4-25
      Called from Base_test__Test_exn_reraise.callstacker in file "test_exn_reraise.ml" (inlined), line 43, characters 2-17
      Called from Base__Exn.handle_uncaught_aux in file "exn.ml" (inlined), line 113, characters 6-10
      Called from Base__Exn.handle_uncaught in file "exn.ml" (inlined), line 139, characters 2-88
      Called from Base_test__Test_exn_reraise.test.(fun) in file "test_exn_reraise.ml", line 53, characters 4-68
   v}
*)

(* An example good backtrace:
   {v
      Uncaught exception:

        (exn.ml.Reraised reraised ("bad value" (x -1)))

      Raised at Base__Error.raise in file "error.ml" (inlined), line 9, characters 14-30
      Called from Base__Error.raise_s in file "error.ml" (inlined), line 10, characters 19-40
      Called from Base_test__Test_exn_reraise.check_value in file "test_exn_reraise.ml", line 26, characters 16-56
      Called from Base_test__Test_exn_reraise.callstacker.loop.(fun) in file "test_exn_reraise.ml" (inlined), line 39, characters 41-54
      Called from Base_test__Test_exn_reraise.reraise_uncaught in file "test_exn_reraise.ml" (inlined), line 33, characters 6-10
      Called from Base_test__Test_exn_reraise.callstacker.loop in file "test_exn_reraise.ml", line 39, characters 4-55
      Re-raised at Base_test__Test_exn_reraise._Caml_Printexc_raise_with_backtrace in file "test_exn_reraise.ml", line 18, characters 2-79
      Called from Base_test__Test_exn_reraise.reraise_uncaught in file "test_exn_reraise.ml" (inlined), line 34, characters 11-23
      Called from Base_test__Test_exn_reraise.callstacker.loop in file "test_exn_reraise.ml", line 39, characters 4-55
      Called from Base_test__Test_exn_reraise.callstacker.loop in file "test_exn_reraise.ml" (inlined), line 40, characters 4-25
      Called from Base_test__Test_exn_reraise.callstacker.loop in file "test_exn_reraise.ml" (inlined), line 40, characters 4-25
      Called from Base_test__Test_exn_reraise.callstacker in file "test_exn_reraise.ml" (inlined), line 43, characters 2-17
      Called from Base__Exn.handle_uncaught_aux in file "exn.ml" (inlined), line 113, characters 6-10
      Called from Base__Exn.handle_uncaught in file "exn.ml" (inlined), line 139, characters 2-88
      Called from Base_test__Test_exn_reraise.test.(fun) in file "test_exn_reraise.ml", line 53, characters 4-68
   v}*)

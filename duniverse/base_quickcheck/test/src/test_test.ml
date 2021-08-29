open! Import

module type S = Test.S

module Config = Test.Config

let default_config = Test.default_config

let%expect_test "default_config" =
  Ref.set_temporarily sexp_style To_string_hum ~f:(fun () ->
    print_s [%sexp (default_config : Config.t)]);
  [%expect
    {|
    ((seed (Deterministic "an arbitrary but deterministic string"))
     (test_count 10000) (shrink_count 10000)
     (sizes
      (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
       28 29 30 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
       25 26 27 28 29 30 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
       22 23 24 25 26 27 28 29 30 0 1 2 3 4 5 6 ...))) |}]
;;

let run = Test.run
let result = Test.result
let run_exn = Test.run_exn

let%expect_test "run_exn" =
  let module M = struct
    type t = bool option list [@@deriving sexp_of]

    let quickcheck_generator = Generator.list (Generator.option Generator.bool)
    let quickcheck_shrinker = Shrinker.list (Shrinker.option Shrinker.bool)
  end
  in
  let module M_without_shrinker = struct
    include M

    let quickcheck_shrinker = Shrinker.atomic
  end
  in
  (* success *)
  let count = ref 0 in
  require_does_not_raise [%here] (fun () ->
    Test.run_exn (module M) ~f:(fun _ -> Int.incr count));
  require
    [%here]
    (!count = Test.default_config.test_count)
    ~if_false_then_print_s:(lazy [%message (!count : int)]);
  [%expect {| |}];
  (* failure *)
  let failure list = assert (List.is_sorted list ~compare:[%compare: bool option]) in
  (* large sizes to demonstrate shrinking *)
  let config =
    { Test.default_config with sizes = Sequence.cycle_list_exn [ 10; 20; 30 ] }
  in
  (* simple failure *)
  require_does_raise [%here] ~hide_positions:true (fun () ->
    Test.run_exn ~config ~f:failure (module M));
  [%expect
    {|
    ("Base_quickcheck.Test.run: test failed"
     (input ((false) ()))
     (error "Assert_failure test_test.ml:LINE:COL")) |}];
  (* failure without shrinking *)
  require_does_raise [%here] ~hide_positions:true (fun () ->
    Test.run_exn ~config ~f:failure (module M_without_shrinker));
  [%expect
    {|
    ("Base_quickcheck.Test.run: test failed"
     (input (() () (false) (true) (false) ()))
     (error "Assert_failure test_test.ml:LINE:COL")) |}];
  (* failure from examples *)
  require_does_raise [%here] ~hide_positions:true (fun () ->
    Test.run_exn
      ~config
      ~f:failure
      ~examples:[ [ Some true; Some true; None; Some true; Some true ] ]
      (module M));
  [%expect
    {|
    ("Base_quickcheck.Test.run: test failed"
     (input ((true) ()))
     (error "Assert_failure test_test.ml:LINE:COL")) |}];
  (* failure from examples without shrinking *)
  require_does_raise [%here] ~hide_positions:true (fun () ->
    Test.run_exn
      ~config
      ~f:failure
      (module M_without_shrinker)
      ~examples:[ [ Some true; Some true; None; Some true; Some true ] ]);
  [%expect
    {|
    ("Base_quickcheck.Test.run: test failed"
     (input ((true) (true) () (true) (true)))
     (error "Assert_failure test_test.ml:LINE:COL")) |}]
;;

let with_sample = Test.with_sample
let with_sample_exn = Test.with_sample_exn

let%expect_test "with_sample_exn" =
  let generator = Generator.list (Generator.option Generator.bool) in
  with_sample_exn
    generator
    ~config:{ Test.default_config with test_count = 20 }
    ~f:(fun sample ->
      Sequence.iter sample ~f:(fun value ->
        Core_kernel.print_s [%sexp (value : bool option list)]));
  [%expect
    {|
    ()
    ()
    (() (true))
    (() (true) (false))
    ()
    ((true))
    ((true))
    ((true))
    ()
    (() (false) (true))
    (() () (false) (true) () (true) () ())
    ((true) () () () (true) (false) ())
    (() () (true) () () (false) (false))
    (() () (true) (true) (false) () (true) ())
    ()
    ()
    ((true) (true) () (false) (false) () () () () (true) () () (true) (false)
     (false) ())
    ((true) (false) (true))
    ((false) () () (false) () () () (false) (false) () (true) () () () ()
     (false))
    () |}]
;;

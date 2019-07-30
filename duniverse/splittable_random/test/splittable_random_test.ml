open! Core_kernel
open  Expect_test_helpers_kernel

let%expect_test "bool fairness" =
  let open Int.O in
  let trial_count = 1_000 in
  let failures = ref [] in
  for seed = 1 to trial_count do
    let state = Splittable_random.State.of_int seed in
    let true_count  = ref 0 in
    let false_count = ref 0 in
    for _ = 1 to 1_000 do
      if Splittable_random.bool state
      then Int.incr true_count
      else Int.incr false_count
    done;
    let diff = Int.abs (!true_count - !false_count) in
    (* Note that diff > 100 is a 3.2-sigma event, and should occur with probability
       roughly 1 in 1000. *)
    if diff > 100 then begin
      let sexp = [%message (seed : int) (true_count : int ref) (false_count : int ref)] in
      failures := sexp :: !failures
    end;
  done;
  let failure_count = List.length !failures in
  let failure_rate =
    Percent.of_mult (Float.of_int failure_count /. Float.of_int trial_count)
  in
  print_s [%message (failure_rate : Percent.t) (failures : Sexp.t list ref)];
  require [%here] (Percent.( < ) failure_rate (Percent.of_percentage 1.));
  [%expect {|
    ((failure_rate 10bp)
     (failures ((
       (seed        573)
       (true_count  551)
       (false_count 449))))) |}];
;;

let%test_module "int64" =
  (module struct
    open Int64.O

    let bounds =
      [ Int64.min_value
      ; Int64.min_value + 1L
      ; Int64.min_value + 2L
      ; -1_000_000_000L
      ; -1_000_000L
      ; -1_000L
      ; -100L
      ; -10L
      ; -2L
      ; -1L
      ; 0L
      ; 1L
      ; 2L
      ; 10L
      ; 100L
      ; 1_000L
      ; 1_000_000L
      ; 1_000_000_000L
      ; Int64.max_value - 2L
      ; Int64.max_value - 1L
      ; Int64.max_value
      ]

    let%test_unit "bounds" =
      let open Int64.O in
      let state = Splittable_random.State.of_int 0 in
      List.iter bounds ~f:(fun lo ->
        List.iter bounds ~f:(fun hi ->
          if lo <= hi then
            for _ = 1 to 1_000 do
              let choice = Splittable_random.int64 state ~lo ~hi in
              if choice < lo || choice > hi then
                Error.raise_s
                  [%message
                    "out of bounds"
                      (choice : int64)
                      (lo     : int64)
                      (hi     : int64)]
            done))

    let%test_unit "coverage" =
      let state = Splittable_random.State.of_int 0 in
      List.iter [1L;10L;100L;1000L] ~f:(fun range ->
        let lo = 0L               in
        let hi = Int64.pred range in
        for _ = 1 to 100 do
          let count = Array.init (Int64.to_int_exn range) ~f:(fun _ -> 0) in
          for _ = 1 to Int64.to_int_exn (range * 100L) do
            let i = Splittable_random.int64 state ~lo ~hi |> Int64.to_int_exn in
            count.(i) <- Int.succ count.(i)
          done;
          Array.iteri count ~f:(fun value count ->
            if Int.equal count 0 then
              Error.raise_s
                [%message
                  "failed to generate value"
                    (value : int)
                    (lo    : int64)
                    (hi    : int64)])
        done)

    (* This should return values with mean 0 and variance 1 if implementation
       is correct. *)
    let test_bias_of_mean ~lo ~hi ~sample_size state =
      let open Int64.O in
      assert (lo < hi);
      let lof = Int64.to_float lo in
      let hif = Int64.to_float hi in
      let delta = hif -. lof in
      let draw () =
        (Int64.to_float (Splittable_random.int64 state ~lo ~hi) -. lof) /. delta
      in
      let rec loop iters accum =
        if iters <= 0L
        then accum
        else loop (iters - 1L) (accum +. draw ())
      in
      let sum = loop sample_size 0. in
      let mean = sum /. Int64.to_float sample_size in
      let n = delta +. 1. in
      (* We have n evenly spaced values from 0 to 1 inclusive, each with probability 1/n.
         This has variance (n+1)/(12(n-1)) per draw.  *)
      let standard_error =
        Float.sqrt ((n +. 1.) /. (12. *. (n -. 1.) *. Int64.to_float sample_size))
      in
      (mean -. 0.5) /. standard_error

    let%test_unit "bias" =
      let open Float.O in
      let hi = 3689348814741910528L in (* about 0.4 * max_int *)
      let z =
        test_bias_of_mean ~lo:0L ~hi ~sample_size:1000L (Splittable_random.State.of_int 0)
      in
      assert (Caml.abs_float z < 3.)

  end)

let%test_module "float" =
  (module struct

    let bounds =
      [ Float.neg Float.max_finite_value
      ; -1_000_000_000.
      ; -1.
      ; -0.000_000_001
      ; 0.
      ; 0.000_000_001
      ; 1.
      ; 1_000_000_000.
      ; Float.max_finite_value
      ]

    let%test_unit "bounds" =
      let open Float.O in
      let state = Splittable_random.State.of_int 0 in
      List.iter bounds ~f:(fun lo ->
        List.iter bounds ~f:(fun hi ->
          if lo < hi then
            for _ = 1 to 1000 do
              let float = Splittable_random.float state ~lo ~hi in
              if float < lo || float > hi then
                Error.raise_s
                  [%message
                    "float out of bounds"
                      (float : float)
                      (lo    : float)
                      (hi    : float)]
            done))

    let%test_unit "coverage" =
      let open Float.O in
      let state = Splittable_random.State.of_int 0 in
      List.iter bounds ~f:(fun lo ->
        List.iter bounds ~f:(fun hi ->
          if lo < hi then
            for _ = 1 to 100 do
              let hi'  = (lo *. 0.01) +. (hi *. 0.99) in
              let lo'  = (lo *. 0.99) +. (hi *. 0.01) in
              let mid1 = (lo *. 0.51) +. (hi *. 0.49) in
              let mid2 = (lo *. 0.49) +. (hi *. 0.51) in
              let saw_hi  = ref false in
              let saw_lo  = ref false in
              let saw_mid = ref false in
              for _ = 1 to 1000 do
                let float = Splittable_random.float state ~lo ~hi in
                if float <                 lo'  then saw_lo  := true;
                if float >                 hi'  then saw_hi  := true;
                if float > mid1 && float < mid2 then saw_mid := true;
              done;
              if not (!saw_lo && !saw_mid && !saw_hi) then
                Error.raise_s
                  [%message
                    "did not get coverage of lo, mid, and hi values"
                      (lo       : float)
                      (hi       : float)
                      (!saw_lo  : bool)
                      (!saw_hi  : bool)
                      (!saw_mid : bool)]
            done))

    let%expect_test "error cases" =
      let state = Splittable_random.State.of_int 0 in
      let test lo hi =
        require_does_raise [%here] (fun () ->
          Splittable_random.float state ~lo ~hi)
      in
      (* NaN bounds *)
      test Float.nan 0.;
      [%expect {|
        ("float: bounds are not finite numbers"
          (lo NAN)
          (hi 0)) |}];
      test 0. Float.nan;
      [%expect {|
        ("float: bounds are not finite numbers"
          (lo 0)
          (hi NAN)) |}];
      (* infinite bounds *)
      test Float.neg_infinity 0.;
      [%expect {|
        ("float: bounds are not finite numbers"
          (lo -INF)
          (hi 0)) |}];
      test 0. Float.infinity;
      [%expect {|
        ("float: bounds are not finite numbers"
          (lo 0)
          (hi INF)) |}];
      (* crossed bounds *)
      test 2. 1.;
      [%expect {|
        ("float: bounds are crossed"
          (lo 2)
          (hi 1)) |}];
    ;;

  end)

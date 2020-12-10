(** This module implements "Fast Splittable Pseudorandom Number Generators" by Steele et.
    al. (1).  The paper's algorithm provides decent randomness for most purposes, but
    sacrifices cryptographic-quality randomness in favor of performance.  The original
    implementation was tested with DieHarder and BigCrush; see the paper for details.

    Our implementation is a port from Java to OCaml of the paper's algorithm.  Other than
    the choice of initial seed for [create], our port should be faithful.  We have not
    re-run the DieHarder or BigCrush tests on our implementation.  Our port is also not as
    performant as the original; two factors that hurt us are boxed [int64] values and lack
    of a POPCNT primitive.

    (1) http://2014.splashcon.org/event/oopsla2014-fast-splittable-pseudorandom-number-generators
    (also mirrored at http://gee.cs.oswego.edu/dl/papers/oopsla14.pdf)

    Beware when implementing this interface; it is easy to implement a [split] operation
    whose output is not as "independent" as it seems (2).  This bug caused problems for
    Haskell's Quickcheck library for a long time.

    (2) Schaathun, "Evaluation of splittable pseudo-random generators", JFP 2015.
    http://www.hg.schaathun.net/research/Papers/hgs2015jfp.pdf
*)

open! Base
open  Int64.O

let is_odd x = x lor 1L = x

let popcount = Int64.popcount

module State = struct

  type t =
    { mutable seed : int64
    ; odd_gamma    : int64
    }

  let golden_gamma = 0x9e37_79b9_7f4a_7c15L

  let of_int seed =
    { seed      = Int64.of_int seed
    ; odd_gamma = golden_gamma
    }

  let copy { seed ; odd_gamma } = { seed ; odd_gamma }

  let mix_bits z n =
    z lxor (z lsr n)

  let mix64 z =
    let z = (mix_bits z 33) * 0xff51_afd7_ed55_8ccdL in
    let z = (mix_bits z 33) * 0xc4ce_b9fe_1a85_ec53L in
    mix_bits z 33

  let mix64_variant13 z =
    let z = (mix_bits z 30) * 0xbf58_476d_1ce4_e5b9L in
    let z = (mix_bits z 27) * 0x94d0_49bb_1331_11ebL in
    mix_bits z 31

  let mix_odd_gamma z =
    let z = (mix64_variant13 z) lor 1L in
    let n = popcount (z lxor (z lsr 1)) in
    (* The original paper uses [>=] in the conditional immediately below; however this is
       a typo, and we correct it by using [<]. This was fixed in response to [1] and [2].

       [1] https://github.com/janestreet/splittable_random/issues/1
       [2] http://www.pcg-random.org/posts/bugs-in-splitmix.html
    *)
    if Int.( < ) n 24
    then z lxor 0xaaaa_aaaa_aaaa_aaaaL
    else z

  let%test_unit "odd gamma" =
    for input = -1_000_000 to 1_000_000 do
      let output = mix_odd_gamma (Int64.of_int input) in
      if not (is_odd output) then
        Error.raise_s [%message
          "gamma value is not odd"
            (input  : int)
            (output : int64)]
    done

  let next_seed t =
    let next = t.seed + t.odd_gamma in
    t.seed <- next;
    next

  let of_seed_and_gamma ~seed ~gamma =
    let seed      = mix64         seed  in
    let odd_gamma = mix_odd_gamma gamma in
    { seed; odd_gamma }

  let random_int64 random_state =
    Random.State.int64_incl random_state Int64.min_value Int64.max_value

  let create random_state =
    let seed  = random_int64 random_state in
    let gamma = random_int64 random_state in
    of_seed_and_gamma ~seed ~gamma

  let split t =
    let seed  = next_seed t in
    let gamma = next_seed t in
    of_seed_and_gamma ~seed ~gamma

  let next_int64 t = mix64 (next_seed t)

  (* [perturb] is not from any external source, but provides a way to mix in external
     entropy with a pseudo-random state. *)
  let perturb t salt =
    let next = t.seed + mix64 (Int64.of_int salt) in
    t.seed <- next

end

let bool state = is_odd (State.next_int64 state)

(* We abuse terminology and refer to individual values as biased or unbiased.  More
   properly, what is unbiased is the sampler that results if we keep only these "unbiased"
   values. *)
let remainder_is_unbiased
      ~draw
      ~remainder
      ~draw_maximum
      ~remainder_maximum
  =
  let open Int64.O in
  draw - remainder <= draw_maximum - remainder_maximum

let%test_unit "remainder_is_unbiased" =
  (* choosing a range of 10 values based on a range of 105 values *)
  let draw_maximum = 104L in
  let remainder_maximum = 9L in
  let is_unbiased draw =
    let remainder = Int64.rem draw (Int64.succ remainder_maximum) in
    remainder_is_unbiased ~draw ~remainder ~draw_maximum ~remainder_maximum
  in
  for i = 0 to 99 do
    [%test_result: bool]
      (is_unbiased (Int64.of_int i))
      ~expect:true
      ~message:(Int.to_string i)
  done;
  for i = 100 to 104 do
    [%test_result: bool]
      (is_unbiased (Int64.of_int i))
      ~expect:false
      ~message:(Int.to_string i)
  done

(* This implementation of bounded randomness is adapted from [Random.State.int*] in the
   OCaml standard library.  The purpose is to use the minimum number of calls to
   [next_int64] to produce a number uniformly chosen within the given range. *)
let int64 =
  let open Int64.O in
  let rec between state ~lo ~hi =
    let draw = State.next_int64 state in
    if lo <= draw && draw <= hi
    then draw
    else between state ~lo ~hi
  in
  let rec non_negative_up_to state maximum =
    let draw = State.next_int64 state land Int64.max_value in
    let remainder = Int64.rem draw (Int64.succ maximum) in
    if remainder_is_unbiased
         ~draw
         ~remainder
         ~draw_maximum:Int64.max_value
         ~remainder_maximum:maximum
    then remainder
    else non_negative_up_to state maximum
  in
  fun state ~lo ~hi ->
    if lo > hi then begin
      Error.raise_s [%message "int64: crossed bounds" (lo : int64) (hi : int64)]
    end;
    let diff = hi - lo in
    if diff = Int64.max_value
    then ((State.next_int64 state) land Int64.max_value) + lo
    else if diff >= 0L
    then (non_negative_up_to state diff) + lo
    else between state ~lo ~hi

let int state ~lo ~hi =
  let lo = Int64.of_int lo in
  let hi = Int64.of_int hi in
  (* truncate unneeded bits *)
  Int64.to_int_trunc (int64 state ~lo ~hi)

let int32 state ~lo ~hi =
  let lo = Int64.of_int32 lo in
  let hi = Int64.of_int32 hi in
  (* truncate unneeded bits *)
  Int64.to_int32_trunc (int64 state ~lo ~hi)

let nativeint state ~lo ~hi =
  let lo = Int64.of_nativeint lo in
  let hi = Int64.of_nativeint hi in
  (* truncate unneeded bits *)
  Int64.to_nativeint_trunc (int64 state ~lo ~hi)

let int63 state ~lo ~hi =
  let lo = Int63.to_int64 lo in
  let hi = Int63.to_int64 hi in
  (* truncate unneeded bits *)
  Int63.of_int64_trunc (int64 state ~lo ~hi)

let double_ulp = 2. **. -53.

let%test_unit "double_ulp" =
  let open Float.O in
  match Word_size.word_size with
  | W64 ->
    assert (1.0 -.  double_ulp         < 1.0);
    assert (1.0 -. (double_ulp /. 2.0) = 1.0)
  | W32 ->
    (* 32-bit OCaml uses a 64-bit float representation but 80-bit float instructions, so
       rounding works differently due to the conversion back and forth. *)
    assert (1.0 -.  double_ulp         <  1.0);
    assert (1.0 -. (double_ulp /. 2.0) <= 1.0)

let unit_float_from_int64 int64 =
  (Int64.to_float (int64 lsr 11)) *. double_ulp

let%test_unit "unit_float_from_int64" = begin
  let open Float.O in
  assert (unit_float_from_int64 0x0000_0000_0000_0000L = 0.);
  assert (unit_float_from_int64 0xffff_ffff_ffff_ffffL < 1.0);
  assert (unit_float_from_int64 0xffff_ffff_ffff_ffffL = (1.0 -. double_ulp));
end

let unit_float state =
  unit_float_from_int64 (State.next_int64 state)

(* Note about roundoff error:

   Although [float state ~lo ~hi] is nominally inclusive of endpoints, we are relying on
   the fact that [unit_float] never returns 1., because there are pairs [(lo,hi)] for
   which [lo +. 1. *. (hi -. lo) > hi].  There are also pairs [(lo,hi)] and values of [x]
   with [x < 1.] such that [lo +. x *. (hi -. lo) = hi], so it would not be correct to
   document this as being exclusive of [hi].
*)
let float =
  let rec finite_float state ~lo ~hi =
    let range = hi -. lo in
    if Float.is_finite range
    then (lo +. (unit_float state *. range))
    else begin
      (* If [hi - lo] is infinite, then [hi + lo] is finite because [hi] and [lo] have
         opposite signs. *)
      let mid = (hi +. lo) /. 2. in
      if bool state
      (* Depending on rounding, the recursion with [~hi:mid] might be inclusive of [mid],
         which would mean the two cases overlap on [mid]. The alternative is to increment
         or decrement [mid] using [one_ulp] in either of the calls, but then if the first
         case is exclusive we leave a "gap" between the two ranges. There's no perfectly
         uniform solution, so we use the simpler code that does not call [one_ulp]. *)
      then finite_float state ~lo ~hi:mid
      else finite_float state ~lo:mid ~hi
    end
  in
  fun state ~lo ~hi ->
    if not (Float.is_finite lo && Float.is_finite hi)
    then begin
      raise_s [%message
        "float: bounds are not finite numbers"
          (lo : float)
          (hi : float)]
    end;
    if Float.( > ) lo hi
    then begin
      raise_s [%message
        "float: bounds are crossed"
          (lo : float)
          (hi : float)]
    end;
    finite_float state ~lo ~hi

let%bench_fun "unit_float_from_int64" =
  let int64 = 1L in
  fun () -> unit_float_from_int64 int64

module Log_uniform = struct
  module Make (M : sig include Int.S val uniform : State.t -> lo:t -> hi:t -> t end) : sig
    val log_uniform : State.t -> lo:M.t -> hi:M.t -> M.t
  end = struct
    open M

    let bits_to_represent t =
      assert (t >= zero);
      let t = ref t in
      let n = ref 0 in
      while !t > zero do
        t := shift_right !t 1;
        Int.incr n;
      done;
      !n

    let%test_unit "bits_to_represent" =
      let test n expect = [%test_result: int] (bits_to_represent n) ~expect in
      test (M.of_int_exn 0)   0;
      test (M.of_int_exn 1)   1;
      test (M.of_int_exn 2)   2;
      test (M.of_int_exn 3)   2;
      test (M.of_int_exn 4)   3;
      test (M.of_int_exn 5)   3;
      test (M.of_int_exn 6)   3;
      test (M.of_int_exn 7)   3;
      test (M.of_int_exn 8)   4;
      test (M.of_int_exn 100) 7;
      test M.max_value (Int.pred M.num_bits);
    ;;

    let min_represented_by_n_bits n =
      if Int.equal n 0
      then zero
      else shift_left one (Int.pred n)

    let%test_unit "min_represented_by_n_bits" =
      let test n expect = [%test_result: M.t] (min_represented_by_n_bits n) ~expect in
      test 0 (M.of_int_exn 0);
      test 1 (M.of_int_exn 1);
      test 2 (M.of_int_exn 2);
      test 3 (M.of_int_exn 4);
      test 4 (M.of_int_exn 8);
      test 7 (M.of_int_exn 64);
      test (Int.pred M.num_bits) (M.shift_right_logical M.min_value 1);
    ;;

    let max_represented_by_n_bits n =
      pred (shift_left one n)

    let%test_unit "max_represented_by_n_bits" =
      let test n expect = [%test_result: M.t] (max_represented_by_n_bits n) ~expect in
      test 0 (M.of_int_exn 0);
      test 1 (M.of_int_exn 1);
      test 2 (M.of_int_exn 3);
      test 3 (M.of_int_exn 7);
      test 4 (M.of_int_exn 15);
      test 7 (M.of_int_exn 127);
      test (Int.pred M.num_bits) M.max_value;
    ;;

    let log_uniform state ~lo ~hi =
      let min_bits = bits_to_represent lo in
      let max_bits = bits_to_represent hi in
      let bits = int state ~lo:min_bits ~hi:max_bits in
      uniform state
        ~lo:(min_represented_by_n_bits bits |> max lo)
        ~hi:(max_represented_by_n_bits bits |> min hi)
  end

  module For_int       = Make (struct include Int       let uniform = int       end)
  module For_int32     = Make (struct include Int32     let uniform = int32     end)
  module For_int63     = Make (struct include Int63     let uniform = int63     end)
  module For_int64     = Make (struct include Int64     let uniform = int64     end)
  module For_nativeint = Make (struct include Nativeint let uniform = nativeint end)

  let int       = For_int.log_uniform
  let int32     = For_int32.log_uniform
  let int63     = For_int63.log_uniform
  let int64     = For_int64.log_uniform
  let nativeint = For_nativeint.log_uniform
end


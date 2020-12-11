open! Core_kernel
module M = Limiter.Token_bucket

let%bench_fun "always limited" =
  let t =
    M.create_exn ~now:(Time_ns.now ()) ~burst_size:1 ~sustained_rate_per_sec:1. ()
  in
  let now = ref (Time_ns.now ()) in
  fun () -> M.try_take ~now:!now t 1
;;

let%bench_fun "never limited" =
  let now = ref (Time_ns.now ()) in
  let t = M.create_exn ~now:!now ~burst_size:1 ~sustained_rate_per_sec:1. () in
  let second = Time_ns.Span.of_sec 1.1 in
  fun () ->
    now := Time_ns.add !now second;
    M.try_take ~now:!now t 1
;;

open Core
open Async

open Mirage_crypto_rng

let src = Logs.Src.create "mirage-crypto-rng-async" ~doc:"Mirage crypto RNG Async"
module Log = (val Logs.src_log src : Logs.LOG)

let running = ref false

let ns_since_epoch time_source () =
  Synchronous_time_source.now time_source
  |> Time_ns.to_int_ns_since_epoch
  |> Int64.of_int

let periodically_collect_cpu_entropy time_source span =
  match Entropy.cpu_rng with
  | Error `Not_supported -> ()
  | Ok cpu_rng ->
    Synchronous_time_source.run_at_intervals
      time_source
      span
      (cpu_rng None)

let periodically_collect_getrandom_entropy time_source span =
  let source = Entropy.register_source "getrandom" in
  Synchronous_time_source.run_at_intervals
    time_source
    span
    (fun () ->
      let per_pool = 8 in
      let size = per_pool * pools None in
      let random = Mirage_crypto_rng_unix.getrandom size in
      let idx = ref 0 in
      let f () =
        incr idx;
        Cstruct.sub random (per_pool * (pred !idx)) per_pool
      in
      Entropy.feed_pools None source f)

let read_cpu_counter_at_the_start_of_every_cycle () =
  Scheduler.Expert.run_every_cycle_start
    (Entropy.timer_accumulator None)

let getrandom_init i =
  let data = Mirage_crypto_rng_unix.getrandom 128 in
  Entropy.header i data

let initialize ?g ?time_source ?(sleep = Time_ns.Span.of_int_sec 1) generator =
  let time_source =
    Option.value ~default:(Synchronous_time_source.wall_clock ()) time_source
  in
  if !running then
    Log.debug
      (fun m -> m "Mirage_crypto_rng_async.initialize has already been called, \
                   ignoring this call.")
  else begin
    (try
       let _ = default_generator () in
       Log.warn (fun m -> m "Mirage_crypto_rng.default_generator has already \
                             been set (but not via \
                             Mirage_crypto_rng_async.initialize). Please check \
                             that this is intentional");
     with
       No_default_generator -> ());
    running := true;
    let seed =
      (* getrandom_init might block, but this is initialization so prima facie, it
         doesn't matter. *)
      let init =
        Entropy.[ bootstrap ; whirlwind_bootstrap ; bootstrap ; getrandom_init ]
      in
      List.mapi ~f:(fun i f -> f i) init |> Cstruct.concat
    in
    let rng = 
      create ?g ~seed ~time:(ns_since_epoch time_source) generator
    in
    set_default_generator rng;
    periodically_collect_cpu_entropy time_source sleep;
    periodically_collect_getrandom_entropy 
      time_source 
      (Time_ns.Span.scale_int sleep 10);
    read_cpu_counter_at_the_start_of_every_cycle ();
  end

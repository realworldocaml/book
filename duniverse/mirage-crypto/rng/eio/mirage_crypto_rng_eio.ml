
open Mirage_crypto_rng

type env = <
  clock: Eio.Time.clock;
  secure_random: Eio.Flow.source;
>

let src = Logs.Src.create "mirage-crypto-rng-eio" ~doc:"Mirage crypto RNG Eio"
module Log = (val Logs.src_log src: Logs.LOG)

let getrandom env i =
  let buf = Cstruct.create i in
  Eio.Flow.read_exact env#secure_random buf;
  buf

let getrandom_init env i =
  let data = getrandom env 128 in
  Entropy.header i data

let rec periodic env f delta =
  f ();
  Eio.Time.sleep env#clock (Duration.to_f delta);
  periodic env f delta

let periodically_feed_entropy env delta source =
  let task () =
    let per_pool = 8 in
    let size = per_pool * pools None in
    let random = getrandom env size in
    let idx = ref 0 in
    let f () =
      incr idx;
      Cstruct.sub random (per_pool * (pred !idx)) per_pool
    in
    Entropy.feed_pools None source f
  in
  periodic env task delta

let rdrand_task env delta =
  match Entropy.cpu_rng with
  | Error `Not_supported -> []
  | Ok cpu_rng -> [ fun () -> periodic env (cpu_rng None) delta ]

let running = ref false

let run
    ?g
    ?(sleep = Duration.of_sec 1)
    generator
    env
    fn
  =
    if !running then begin
      Log.debug
        (fun m -> m "Mirage_crypto_rng_eio.initialize has already been called, \
                     ignoring this call.");
      fn ()
    end
    else begin
      running := true;
      Fun.protect
        ~finally:(fun () ->
            running := false;
            unset_default_generator ())
        (fun () ->
          (try
            let _ = default_generator () in
            Log.warn (fun m -> m "Mirage_crypto_rng.default_generator has already \
                                  been set, check that this call is intentional");
           with
            No_default_generator -> ());
          let seed =
            let init =
              Entropy.[ bootstrap ; whirlwind_bootstrap ; bootstrap ; getrandom_init env ] in
            List.mapi (fun i f -> f i) init |> Cstruct.concat
          in
          let rng = create ?g ~seed ~time:Mtime_clock.elapsed_ns generator in
          set_default_generator rng;
          let source = Entropy.register_source "getrandom" in
          let feed_entropy () = periodically_feed_entropy env (Int64.mul sleep 10L) source in
          Eio.Fiber.any (rdrand_task env sleep @ [feed_entropy ; fn]))
    end

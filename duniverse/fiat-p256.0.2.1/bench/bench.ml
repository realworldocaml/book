let crypto_random_bytes n =
  let ic = Stdlib.open_in_bin "/dev/urandom" in
  let s = Stdlib.really_input_string ic n in
  close_in ic;
  Cstruct.of_string s

let generate_key_pair () = Fiat_p256.gen_key ~rng:crypto_random_bytes

let bench_dh () =
  let scalar, _ = generate_key_pair () in
  let _, point = generate_key_pair () in
  let run () : (Cstruct.t, Fiat_p256.error) result =
    Fiat_p256.key_exchange scalar point
  in
  Benchmark.throughputN 1 [ ("P-256", run, ()) ]

let () =
  let open Benchmark.Tree in
  register @@ "Fiat" @>>> [ "dh" @> lazy (bench_dh ()) ]

let () = Benchmark.Tree.run_global ()

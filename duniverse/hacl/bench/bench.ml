let read_urandom n =
  let ic = Stdlib.open_in_bin "/dev/urandom" in
  let s = Stdlib.really_input_string ic n in
  close_in ic;
  Cstruct.of_string s

let generate_key_pair () = Hacl_x25519.gen_key ~rng:read_urandom

let bench_dh () =
  let priv, _ = generate_key_pair () in
  let _, pub = generate_key_pair () in
  let run () : (Cstruct.t, _) result = Hacl_x25519.key_exchange priv pub in
  Benchmark.throughputN 1 [ ("X25519", run, ()) ]

let () =
  let open Benchmark.Tree in
  register @@ "Hacl" @>>> [ "dh" @> lazy (bench_dh ()) ]

let () = Benchmark.Tree.run_global ()

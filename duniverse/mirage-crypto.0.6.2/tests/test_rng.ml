
let all0 cs =
  let c = ref 0 in
  let l = Cstruct.len cs in
  for i = 0 to pred l do
    if Cstruct.get_uint8 cs i = 0 then incr c
  done;
  l = !c

let () =
  Printf.printf "foo\n%!";
  Mirage_crypto_rng_unix.initialize ();
  let rec one z n = function
    | 0 -> z
    | c ->
      let data = Mirage_crypto_rng.generate n in
      Cstruct.hexdump data;
      let z' = if all0 data then succ z else z in
      one z' n (pred c)
  in
  let iterations = 1_000 in
  let all0 = one 0 260 iterations in
  if all0 = iterations then
    Printf.printf "all 0\n"
  else
    Printf.printf "%d iterations\n" iterations

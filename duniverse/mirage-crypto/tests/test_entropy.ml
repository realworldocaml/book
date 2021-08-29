
let data = ref Cstruct.empty

let cpu_bootstrap_check () =
  match Mirage_crypto_rng.Entropy.cpu_rng_bootstrap with
  | Error `Not_supported -> print_endline "no CPU RNG available"
  | Ok cpu_rng_bootstrap ->
    match cpu_rng_bootstrap 1 with
    | exception Failure _ -> print_endline "bad CPU RNG"
    | data' ->
      data := data';
      for i = 0 to 10 do
        try
          let data' = cpu_rng_bootstrap 1 in
          if Cstruct.equal !data data' then begin
            Cstruct.hexdump data';
            failwith ("same data from CPU bootstrap at " ^ string_of_int i);
          end;
          data := data'
        with Failure _ -> print_endline ("CPU RNG failed at " ^ string_of_int i)
      done

let whirlwind_bootstrap_check () =
  for i = 0 to 10 do
    let data' = Mirage_crypto_rng.Entropy.whirlwind_bootstrap 1 in
    if Cstruct.equal !data data' then begin
      Cstruct.hexdump data';
      failwith ("same data from whirlwind bootstrap at " ^ string_of_int i);
    end;
    data := data'
  done

let timer_check () =
  for i = 0 to 10 do
    let data' = Mirage_crypto_rng.Entropy.interrupt_hook () () in
    if Cstruct.equal !data data' then begin
      Cstruct.hexdump data';
      failwith ("same data from timer at " ^ string_of_int i);
    end;
    data := data'
  done

let () =
  timer_check ();
  cpu_bootstrap_check ();
  whirlwind_bootstrap_check ();
  print_endline "test entropy OK"

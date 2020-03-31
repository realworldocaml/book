let exit_success = 0
let exit_failure = 1

external random_seed : unit -> int array = "caml_sys_random_seed"

let pp_int_array ppf arr =
  Fmt.pf ppf "[|" ;
  for i = 0 to pred (Array.length arr) do Fmt.pf ppf "%d;" arr.(i) done ;
  Fmt.pf ppf "|]"

let () =
  let random_seed = random_seed () in
  Fmt.pr "Random: %a.\n%!" pp_int_array random_seed ;
  Random.full_init random_seed

let random length =
  let get _ =
    match Random.int (10 + 26 + 26) with
    | n when n < 10 -> Char.(chr (code '0' + n))
    | n when n < 10 + 26 -> Char.(chr (code 'a' + n - 10))
    | n -> Char.(chr (code 'A' + n - 10 - 26)) in
  String.init length get

let hash_eq_0 = random 4096
let hash_eq_1 = Bytes.to_string (Bytes.of_string hash_eq_0)

let () = assert (hash_eq_0 != hash_eq_1)
let () = assert (hash_eq_0 = hash_eq_1)

let hash_neq_0 = random 4096
let hash_neq_1 =
  let rec go limit =
    if limit <= 0 then failwith "Impossible to generate different hashes." ;
    let res = random 4096 in
    if res = hash_neq_0 then go (pred limit) else res in
  go 10

let () = assert (hash_neq_0 <> hash_neq_1)

let error_msgf fmt = Fmt.kstrf (fun err -> Error (`Msg err)) fmt

let merge m0 m1 =
  let cons_0 r = [| 0.; r.(0); r.(1) |] in
  let cons_1 r = [| 1.; r.(0); r.(1) |] in
  Array.(append (map cons_0 m0) (map cons_1 m1))

let test_spss fn_0 fn_1 =
  Fmt.pr "> Start benchmarks on [fn⁰].\n%!" ;
  let m0 = Benchmark.run fn_0 in
  Fmt.pr "> Start benchmarks on [fn¹].\n%!" ;
  let m1 = Benchmark.run fn_1 in
  Fmt.pr "> Merge results.\n%!" ;
  let m = merge m0 m1 in
  let m = Array.map (fun r -> [| r.(0); r.(1); r.(2); r.(0) *. r.(1) |]) m in
  Fmt.pr "> Start linear regression.\n%!" ;
  match Linear_algebra.ols
          (fun m -> m.(2))
          [|(fun m -> m.(0)); (fun m -> m.(1)); (fun m -> m.(3))|]
          m with
  | Ok (estimates, r_square) ->
    if r_square >= 0.95 then Ok estimates
    else error_msgf "r² (%f) is bad" r_square
  | Error (`Msg _) as err -> err

let test_ccea fn_0 fn_1 =
  Fmt.pr "> Start benchmarks on [fn⁰].\n%!" ;
  let m0 = Benchmark.run fn_0 in
  Fmt.pr "> Start benchmarks on [fn¹].\n%!" ;
  let m1 = Benchmark.run fn_1 in
  match Linear_algebra.ols (fun m -> m.(1)) [|(fun m -> m.(0))|] m0,
        Linear_algebra.ols (fun m -> m.(1)) [|(fun m -> m.(0))|] m1 with
  | Ok (estimates_0, r_square_0),
    Ok (estimates_1, r_square_1) ->
    Fmt.epr "> Calculating Z.\n" ;
    let z = (estimates_0.(0) -. estimates_1.(0)) /. sqrt ((r_square_0 ** 2.) +. (r_square_1 ** 2.)) in
    Ok z
  | (Error (`Msg _) as err), Ok _ -> err
  | Ok _, (Error (`Msg _) as err) -> err
  | Error (`Msg err0), Error (`Msg err1) ->
    Fmt.epr "Got errors for while processing both.\n%!" ;
    Fmt.epr "B¹: %s.\n%!" err0 ;
    Fmt.epr "B²: %s.\n%!" err1 ;
    exit exit_failure

let stdlib_eq () = String.equal hash_eq_0 hash_eq_1
let stdlib_neq () = String.equal hash_neq_0 hash_neq_1

let eqaf_eq () = Eqaf.equal hash_eq_0 hash_eq_1
let eqaf_neq () = Eqaf.equal hash_neq_0 hash_neq_1

let stdlib_cmp () = String.compare hash_eq_0 hash_eq_1
let stdlib_ncmp () = String.compare hash_neq_0 hash_neq_1

let eqaf_cmp () = Eqaf.compare_be hash_eq_0 hash_eq_1
let eqaf_ncmp () = Eqaf.compare_be hash_neq_0 hash_neq_1

let ccea fns_0 fns_1 =
  Fmt.pr "> Start to test eqaf (B¹).\n%!" ;
  let eqaf = test_ccea (fst fns_0) (snd fns_0) in
  Fmt.pr "> Start to test string.equal (B²).\n%!" ;
  let stdlib = test_ccea (fst fns_1) (snd fns_1) in
  match eqaf, stdlib with
  | Ok eqaf, Ok stdlib ->
    Ok (eqaf, stdlib)
  | Error (`Msg err), Ok _ ->
    Fmt.epr "Got an error while processing eqaf: %s\n%!" err ;
    Error ()
  | Ok _, Error (`Msg err) ->
    Fmt.epr "Got an error while processing string.equal: %s\n%!" err ;
    Error ()
  | Error (`Msg err0), Error (`Msg err1) ->
    Fmt.epr "Got errors while processing both:\n%!" ;
    Fmt.epr "B¹> %s.\n%!" err0 ;
    Fmt.epr "B²> %s.\n%!" err1 ;
    Error ()

let spss fns_0 fns_1 =
  Fmt.pr "> Start to test eqaf (B¹).\n%!" ;
  let eqaf = test_spss (fst fns_0) (snd fns_0) in
  Fmt.pr "> Start to test string.equal (B²).\n%!" ;
  let stdlib = test_spss (fst fns_1) (snd fns_1) in

  match eqaf, stdlib with
  | Ok eqaf, Ok stdlib ->
    Fmt.pr "eqaf: %f ns/run.\n%!" eqaf.(1) ;
    Fmt.pr "string.equal: %f ns/run.\n%!" stdlib.(1) ;
    Ok (eqaf.(2), stdlib.(2))
  | Error (`Msg err), Ok _ ->
    Fmt.epr "Got an error while processing eqaf: %s\n%!" err ;
    Error ()
  | Ok _, Error (`Msg err) ->
    Fmt.epr "Got an error while processing string.equal: %s\n%!" err ;
    Error ()
  | Error (`Msg err0), Error (`Msg err1) ->
    Fmt.epr "Got errors while processing both:\n%!" ;
    Fmt.epr "B¹> %s.\n%!" err0 ;
    Fmt.epr "B²> %s.\n%!" err1 ;
    Error ()

(* XXX(dinosaure): this program try to compute diff between 2 coefficient
   regressions:

   - 1: time needed to compute equal function on 2 same values ([_eq])
   - 2: time needed to compute equal function on 2 different values ([_neq])

   We have 2 ways to compute it. The first is to compute a regression equation
   which includes group 1 and group 2. A initial regression equation can be done
   to know how long [equal] lasts:

   regression
     /dep time // m.(1)
     /method = enter run // m.(0)

   The first way to compare group 1 ([_eq]) and group 2 ([_neq]): we need to
   insert a dummy variable [kind] where it is equal to [0.0] when it's owned by
   the group 1 and [1.1] is owned by the group 2 (see [cons_*] function).
   Finally, we had a new variable which is the product between [kind] ([m.(0)])
   and [run] ([m.(1)]).

   Finally, we can start to compute a regression equation where [time] will be
   the responder and [kind], [run] and [kind * run] will be predictors:

   regression
     /dep time // m.(2)
     /method = enter kind run (kind * run) // m.(0) m.(1) m.(3)

   Time of [equal] will be available on [estimates.(1)] and diff will be
   available on [estimates.(2)]. [compare_spss] checks r² ([>= 0.95]) and
   main program checks if the diff is between [-30.0] and [30.0].

   The second way to compare group 1 and group 2: it consists to compute basic
   regression equation to know how long [equal] lasts. Then, we will compute [Z]
   which is equal to:

        B¹-B²
   ---------------
   sqrt(r¹² + r²²)

   Where B¹ and B² are regression coefficients for [_eq] and [_neq] and r¹ and r²
   are standard error of B¹ and B². Then, main program, as the first way, checks
   if [Z] is between [-30.0] and [30.0].

   NOTE about virtualization:

   Virtual context (VirtualBox, VMWare, Xen or qemu) can delayed CPU instructions
   and tricks on the time spended to execute them. By this fact, time counter lies
   about time needed to compute [equal] function. So, in a virtual context we can
   have some noises when we record measures (in [Benchmark]). *)

let equal_last_chance () =
  let open Benchmark in
  match ccea (V eqaf_eq, V eqaf_neq) (V stdlib_eq, V stdlib_neq) with
  | Error () -> exit_failure
  | Ok (eqaf, stdlib) ->
    if eqaf >= -30. && eqaf <= 30.
    then ( Fmt.pr "Z¹ = %f, Z² = %f.\n%!" eqaf stdlib ; exit_success )
    else ( Fmt.pr "Z¹ = %f, Z² = %f.\n%!" eqaf stdlib ; exit_failure )

let equal () =
  let open Benchmark in
  match spss (V eqaf_eq, V eqaf_neq) (V stdlib_eq, V stdlib_neq) with
  | Error () -> equal_last_chance ()
  | Ok (eqaf, stdlib) ->
    if eqaf >= -30. && eqaf <= 30.
    then ( Fmt.pr "B¹ = %f, B² = %f.\n%!" eqaf stdlib ; exit_success )
    else
      ( Fmt.pr "Fail with B¹ = %f, B² = %f.\n%!" eqaf stdlib ;
        Fmt.pr "> Start to compute Z.\n%!" ;
        equal_last_chance () )

let compare_last_chance () =
  let open Benchmark in
  match ccea (V eqaf_cmp, V eqaf_ncmp) (V stdlib_cmp, V stdlib_ncmp) with
  | Error () -> exit_failure
  | Ok (eqaf, stdlib) ->
    if eqaf >= -30. && eqaf <= 30.
    then ( Fmt.pr "Z¹ = %f, Z² = %f.\n%!" eqaf stdlib ; exit_success )
    else ( Fmt.pr "Z¹ = %f, Z² = %f.\n%!" eqaf stdlib ; exit_failure )

let compare () =
  let open Benchmark in
  match spss (V eqaf_cmp, V eqaf_ncmp) (V stdlib_cmp, V stdlib_ncmp) with
  | Error () -> compare_last_chance ()
  | Ok (eqaf, stdlib) ->
    if eqaf >= -30. && eqaf <= 30.
    then ( Fmt.pr "B¹ = %f, B² = %f.\n%!" eqaf stdlib ; exit_success )
    else
      ( Fmt.pr "Fail with B¹ = %f, B² = %f.\n%!" eqaf stdlib ;
        compare_last_chance () )

let () =
  let _0 = equal () in
  let _1 = compare () in
  if _0 = exit_success && _1 = exit_success
  then () else exit exit_failure

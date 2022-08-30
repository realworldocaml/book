let exit_success = 0
let exit_failure = 1

external random_seed : unit -> int array = "caml_sys_random_seed"

let pp_int_array ppf arr =
  Fmt.pf ppf "[|" ;
  for i = 0 to pred (Array.length arr) do Fmt.pf ppf "%d;" arr.(i) done ;
  Fmt.pf ppf "|]"

 (* XXX(dinosaure): deterministic generation.
    It appears that some calls of [check/check.exe] does not get same results,
    mostly about [String.*] functions. As we understand implementation of them,
    it's an expected behavior but it puts some noises when we try to introspect
    results on different platforms.
    
    So all inputs are generated with this seed to be able to get as much as we
    can reproducible outputs. *)
let seed = "4EygbdYh+v35vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="
let seed = Base64.decode_exn seed
let seed =
   let res = Array.make (String.length seed / 2) 0 in
   for i = 0 to (String.length seed / 2) - 1
   do res.(i) <- (Char.code seed.[i * 2] lsl 8) lor (Char.code seed.[i * 2 + 1]) done ;
   res

let () =
  let random_seed = seed in
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
let chr_into_hash_eq_0 = hash_eq_0.[Random.int 4096]
let int32_into_hash_eq_0 =
  Unsafe.get_int32_ne (Bytes.of_string hash_eq_0) (Random.int (4096-4))
let int32_into_hash_eq_1 =
  Unsafe.get_int32_ne (Bytes.of_string hash_eq_1) (Random.int (4096-4))
let int14_into_hash_eq_0 =
  Unsafe.get_int32_ne (Bytes.of_string hash_eq_0) (Random.int (4096-4))
  |> (Int32.logand 0xfffl)
let int14_into_hash_eq_1 =
  Unsafe.get_int32_ne (Bytes.of_string hash_eq_1) (Random.int (4096-4))
  |> (Int32.logand 0xfffl)

let () = assert (hash_eq_0 != hash_eq_1)
let () = assert (hash_eq_0 = hash_eq_1)
let () = assert (String.contains hash_eq_0 chr_into_hash_eq_0)

let hash_neq_0 = random 4096
let hash_neq_1 =
  let rec go limit =
    if limit <= 0 then failwith "Impossible to generate different hashes." ;
    let res = random 4096 in
    if res = hash_neq_0 then go (pred limit) else res in
  go 10
let random_chr =
  let rec go limit =
    if limit <= 0 then failwith "Impossible to generate a byte which does not appear into hash_neq_0." ;
    let res = Char.chr (Random.int 256) in
    if not (String.contains hash_neq_0 res) then res else go (pred limit) in
  go 10

let () = assert (hash_neq_0 <> hash_neq_1)
let () = assert (not (String.contains hash_neq_0 random_chr))

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
    Fmt.epr "> Calculating Z.\n%!" ;
    let z = (estimates_0.(0) -. estimates_1.(0)) /. sqrt ((r_square_0 ** 2.) +. (r_square_1 ** 2.)) in
    Ok z
  | (Error (`Msg _) as err), Ok _ -> err
  | Ok _, (Error (`Msg _) as err) -> err
  | Error (`Msg err0), Error (`Msg err1) ->
    Fmt.epr "Got errors for while processing both.\n%!" ;
    Fmt.epr "B¹: %s.\n%!" err0 ;
    Fmt.epr "B²: %s.\n%!" err1 ;
    exit exit_failure

let ccea ~reset ~switch ~name_of_fns_0 ~name_of_fns_1 fns_0 fns_1 =
  Fmt.pr "> Start to test %s (B¹).\n%!" name_of_fns_0 ;
  reset () ;
  let eqaf = test_ccea (fst fns_0) (snd fns_0) in
  switch () ;
  Fmt.pr "> Start to test %s (B²).\n%!" name_of_fns_1 ;
  let stdlib = test_ccea (fst fns_1) (snd fns_1) in
  match eqaf, stdlib with
  | Ok eqaf, Ok stdlib ->
    Ok (eqaf, stdlib)
  | Error (`Msg err), Ok _ ->
    Fmt.epr "Got an error while processing %s: %s\n%!" name_of_fns_0 err ;
    Error ()
  | Ok _, Error (`Msg err) ->
    Fmt.epr "Got an error while processing %s: %s\n%!" name_of_fns_1 err ;
    Error ()
  | Error (`Msg err0), Error (`Msg err1) ->
    Fmt.epr "Got errors while processing both:\n%!" ;
    Fmt.epr "B¹> %s.\n%!" err0 ;
    Fmt.epr "B²> %s.\n%!" err1 ;
    Error ()

let spss ~reset ~switch ~name_of_fns_0 ~name_of_fns_1 fns_0 fns_1 =
  Fmt.pr "> Start to test %s (B¹).\n%!" name_of_fns_0 ;
  reset () ;
  let eqaf = test_spss (fst fns_0) (snd fns_0) in
  switch () ;
  Fmt.pr "> Start to test %s (B²).\n%!" name_of_fns_1 ;
  let stdlib = test_spss (fst fns_1) (snd fns_1) in

  match eqaf, stdlib with
  | Ok eqaf, Ok stdlib ->
    Fmt.pr "%s: %f ns/run.\n%!" name_of_fns_0 eqaf.(1) ;
    Fmt.pr "%s: %f ns/run.\n%!" name_of_fns_1 stdlib.(1) ;
    Ok (eqaf.(2), stdlib.(2))
  | Error (`Msg err), Ok _ ->
    Fmt.epr "Got an error while processing %s: %s\n%!" name_of_fns_0 err ;
    Error ()
  | Ok _, Error (`Msg err) ->
    Fmt.epr "Got an error while processing %s: %s\n%!" name_of_fns_1 err ;
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

   ### Samples

   We have 2 ways to compute it. The first is to compute a regression equation
   which includes group 1 and group 2. A initial regression equation can be done
   to know how long [equal] lasts:

   regression
     /dep time // m.(1)
     /method = enter run // m.(0)

   It's a basic linear regression where we run 1..N times the function with same
   inputs. Then, we have a matrix such as:

   m.(n).(0) <- time
   m.(n).(1) <- run

   Obviously, if our function is /constant-time/, you should have something like:

   y = m.(x).(0) = a * m.(x).(1) + b

   To infer the curve, we use the linear regression for each points. Then, we
   collect same samples but with [_neq] values. Now, the goal is to see that
   [_eq]: y = a * x + b and [_neq]: y = a * x + b are ~ equals. For that, we have
   2 ways.

   ### SPSS

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

   ### CCEA

   The second way to compare group 1 and group 2: it consists to compute basic
   regression equation to know how long [equal] lasts. Then, we will compute [Z]
   which is equal to:

        B¹-B²
   ---------------
   sqrt(r¹² + r²²)

   Where B¹ and B² are regression coefficients for [_eq] and [_neq] and r¹ and r²
   are standard error of B¹ and B². Then, main program, as the first way, checks
   if [Z] is between [-30.0] and [30.0].

   NOTE about SPSS:

   This is the name of a software which explain how to compare results of linear
   regression.

   NOTE about CCEA:

   I don't remmember when I got this name but it seems close to Vuong test.

   NOTE about virtualization:

   Virtual context (VirtualBox, VMWare, Xen or qemu) can delayed CPU instructions
   and tricks on the time spended to execute them. By this fact, time counter lies
   about time needed to compute [equal] function. So, in a virtual context we can
   have some noises when we record measures (in [Benchmark]).

   NOTE about bare-metal:
   
   In a bare-metal context, results are more determinists (but they are not
   completely fixed). In fact, it depends on the system-scheduler which can 
   prioritize an other process while [check/check.exe] is executed. For all of
   these reasons, [check/check.exe] is really fragile and can not work in
   your context - however, a CI with [eqaf] is provided is we surely are aware
   of it and results. *)

module Make (Check : sig 
    type ret

    val eqaf_name : string
    val stdlib_name : string

    val reset : unit -> unit
    val switch : unit -> unit

    val eqaf_true : unit -> ret
    val eqaf_false : unit -> ret

    val stdlib_true : unit -> ret
    val stdlib_false : unit -> ret
  end) = struct
  open Check

  let last_chance () =
    let open Benchmark in
    match ccea
            ~reset:Check.reset ~switch:Check.switch
            ~name_of_fns_0:eqaf_name
            ~name_of_fns_1:stdlib_name
            (V eqaf_true, V eqaf_false)
            (V stdlib_true, V stdlib_false) with
    | Error () -> exit_failure
    | Ok (eqaf, stdlib) ->
      if eqaf >= -30. && eqaf <= 30.
      then ( Fmt.pr "Z¹ = %f, Z² = %f.\n%!" eqaf stdlib ; exit_success )
      else ( Fmt.pr "Z¹ = %f, Z² = %f.\n%!" eqaf stdlib ; exit_failure )
  
  let test () =
    let open Benchmark in
    match spss
            ~reset:Check.reset ~switch:Check.switch
            ~name_of_fns_0:eqaf_name
            ~name_of_fns_1:stdlib_name
            (V eqaf_true, V eqaf_false)
            (V stdlib_true, V stdlib_false) with
    | Error () -> last_chance ()
    | Ok (eqaf, stdlib) ->
      if eqaf >= -30. && eqaf <= 30.
      then ( Fmt.pr "B¹ = %f, B² = %f.\n%!" eqaf stdlib ; exit_success )
      else
        ( Fmt.pr "Fail with B¹ = %f, B² = %f.\n%!" eqaf stdlib ;
          Fmt.pr "> Start to compute Z.\n%!" ;
          last_chance () )
end

module Equal = Make(struct
  type ret = bool

  let eqaf_name = "Eqaf.equal"
  let stdlib_name = "String.equal"

  let reset = ignore and switch = ignore

  let stdlib_true () = String.equal hash_eq_0 hash_eq_1
  let stdlib_false () =
    for _ = 1 to 100
    do let _ = String.equal hash_neq_0 hash_neq_1 in () done ;
    String.equal hash_neq_0 hash_neq_1

  let eqaf_true () = Eqaf.equal hash_eq_0 hash_eq_1
  let eqaf_false () = Eqaf.equal hash_neq_0 hash_neq_1
end)

module Compare = Make(struct
  type ret = int

  let eqaf_name = "Eqaf.compare"
  let stdlib_name = "String.compare"

  let reset = ignore and switch = ignore

  let stdlib_true () = String.compare hash_eq_0 hash_eq_1
  let stdlib_false () =
    for _ = 1 to 100
    do let _ = String.compare hash_neq_0 hash_neq_1 in () done ;
    String.compare hash_neq_0 hash_neq_1
  
  let eqaf_true () = Eqaf.compare_be hash_eq_0 hash_eq_1
  let eqaf_false () = Eqaf.compare_be hash_neq_0 hash_neq_1
end)

module Exists = Make(struct
  type ret = bool

  let eqaf_name = "Eqaf.exists_uint8"
  let stdlib_name = "String.contains"

  let constant = ref (Char.code chr_into_hash_eq_0)
  let reset () = constant := Char.code chr_into_hash_eq_0
  let switch () = constant := Char.code random_chr

  let stdlib_true () = String.contains hash_eq_0 chr_into_hash_eq_0
  let stdlib_false () = String.contains hash_neq_0 random_chr

  let f (v : int) = v = !constant
  let eqaf_true () = Eqaf.exists_uint8 ~f hash_eq_0
  let eqaf_false () = Eqaf.exists_uint8 ~f hash_neq_0
end)

module Find = Make(struct
  type ret = int

  let eqaf_name = "Eqaf.find_uint8"
  let stdlib_name = "String.index"

  let switch () = ()
  let reset () = ()

  let stdlib_true () = String.index hash_eq_0 chr_into_hash_eq_0
  let stdlib_false () = try String.index hash_neq_0 random_chr with Not_found -> (-1)

  let f_hash_eq_0 (v : int) = v = Char.code chr_into_hash_eq_0
  let f_random (v : int) = v = Char.code random_chr
  let eqaf_true () = Eqaf.find_uint8 ~f:f_hash_eq_0 hash_eq_0
  let eqaf_false () = Eqaf.find_uint8 ~f:f_random hash_neq_0
end)

module Divmod32 = Make(struct
  type ret = int32 * int32

  let eqaf_name = "Eqaf.divmod"
  let stdlib_name = "Int32.unsigned_div,Int32.unsigned_rem"


  let switch () = ()
  let reset () = ()

  (* These are here for compat with OCaml <= 4.09
     from >= they can be replaced by
     Int32.unsigned_div
     Int32.unsigned_rem
  *)
  let int32_div_unsigned n d =
    let sub,min_int = Int32.(sub,min_int)in
    let int32_unsigned_compare n m =
        Int32.compare (sub n min_int) (sub m min_int)
    in
    if d < 0_l then
      if int32_unsigned_compare n d < 0 then 0_l else 1_l
    else
      let q =
        let open Int32 in
        shift_left (Int32.div (Int32.shift_right_logical n 1) d) 1 in
      let r = sub n (Int32.mul q d) in
      if int32_unsigned_compare r d >= 0 then Int32.succ q else q
  let int32_rem_unsigned n d =
    Int32.sub n (Int32.mul (int32_div_unsigned n d) d)

  (* TODO *)
  let stdlib_true () =
    let x, m = int32_into_hash_eq_0, int14_into_hash_eq_0 in
    int32_div_unsigned x m,
    int32_rem_unsigned x m
  let stdlib_false () =
    let x, m = int32_into_hash_eq_1, int14_into_hash_eq_1 in
    int32_div_unsigned x m,
    int32_rem_unsigned x m

  let eqaf_true () =
    Eqaf.divmod ~x:int32_into_hash_eq_0 ~m:int14_into_hash_eq_0
  let eqaf_false () =
    Eqaf.divmod ~x:int32_into_hash_eq_1 ~m:int14_into_hash_eq_1
end)

module Ascii_int32 = Make(struct
  type ret = string

  let eqaf_name = "Eqaf.ascii_of_int32"
  let stdlib_name = "Int32.to_string"


  let switch () = ()
  let reset () = ()

  (* TODO setting 0x8000 bit ensures five digits.
     We need a constant amount of digits to specify ~digits because
     we don't have a [Int32.to_string] that left-pads.
     Maybe we can use [Format.sprintf] ?
  *)
  let true_int = Int32.logand 0x8000l int14_into_hash_eq_0
  let false_int = Int32.logand 0x8000l int14_into_hash_eq_1
  let stdlib_true () = Int32.to_string true_int
  let stdlib_false () = Int32.to_string false_int

  let eqaf_true () = Eqaf.ascii_of_int32 ~digits:5 true_int
  let eqaf_false () = Eqaf.ascii_of_int32 ~digits:5 false_int
end)

let limit = 20

let () =
  let rec _0 tried =
    if tried > 20 then invalid_arg "Too many tried for Eqaf.equal" ;
    let res = Equal.test () in
    if res = exit_success then tried else _0 (succ tried) in
  let rec _1 tried =
    if tried > 20 then invalid_arg "Too many tried for Eqaf.compare" ;
    let res = Compare.test () in
    if res = exit_success then tried else _1 (succ tried) in
  let rec _2 tried =
    if tried > 20 then invalid_arg "Too many tried for Eqaf.exists" ;
    let res = Exists.test () in
    if res = exit_success then tried else _2 (succ tried) in
  let rec _3 tried =
    if tried > 20 then invalid_arg "Too many tried for Eqaf.find_uint8" ;
    let res = Find.test () in
    if res = exit_success then tried else _3 (succ tried) in
  let rec _4 tried =
    if tried > 20 then invalid_arg "Too many tried for Eqaf.divmod" ;
    let res = Divmod32.test () in
    if res = exit_success then tried else _4 (succ tried) in
  let pr_bench name value = 
    Fmt.pr {|{"results": [{"name": "check", "metrics": [{"name": "%s", "value": %d}]}]}@.|} name value in
  
  let _0 = _0 1 in
  Fmt.pr "%d trial(s) for Eqaf.equal.\n%!" _0 ;
  pr_bench "equal" _0 ;
  let _1 = _1 1 in
  Fmt.pr "%d trial(s) for Eqaf.compare.\n%!" _1 ;
  pr_bench "compare" _1 ;
  let _2 = _2 1 in
  Fmt.pr "%d trial(s) for Eqaf.exists.\n%!" _2 ;
  pr_bench "exists" _2 ;
  let _3 = _3 1 in
  Fmt.pr "%d trial(s) for Eqaf.find_uint8.\n%!" _3 ;
  pr_bench "find_uint8" _3 ;
  let _4 = _4 1 in
  Fmt.pr "%d trial(s) for Eqaf.divmod.\n%!" _4 ;
  pr_bench "divmod" _4 ;

  exit exit_success
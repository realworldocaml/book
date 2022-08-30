let seed = "4EygbdYh+v35vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="
let seed = Base64.decode_exn seed

let seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done;
  res

let () =
  let random_seed = seed in
  Random.full_init random_seed

let random length =
  let get _ =
    match Random.int (10 + 26 + 26) with
    | n when n < 10 -> Char.(chr (code '0' + n))
    | n when n < 10 + 26 -> Char.(chr (code 'a' + n - 10))
    | n -> Char.(chr (code 'A' + n - 10 - 26))
  in
  String.init length get

open Bechamel
open Toolkit

let hash_eq_0 = random 4096
let hash_eq_1 = Bytes.to_string (Bytes.of_string hash_eq_0)
let chr_into_hash_eq_0 = hash_eq_0.[Random.int 4096]
let hash_neq_0 = random 4096

let hash_neq_1 =
  let rec go limit =
    if limit <= 0 then failwith "Impossible to generate different hashes.";
    let res = random 4096 in
    if res = hash_neq_0 then go (pred limit) else res
  in
  go 10

let random_chr =
  let rec go limit =
    if limit <= 0 then
      failwith
        "Impossible to generate a byte which does not appear into hash_neq_0.";
    let res = Char.chr (Random.int 256) in
    if not (String.contains hash_neq_0 res) then res else go (pred limit)
  in
  go 10

let test_equal0 =
  Test.make ~name:"equal"
    (Staged.stage @@ fun () -> Eqaf.equal hash_eq_0 hash_eq_1)

let test_equal1 =
  Test.make ~name:"not equal"
    (Staged.stage @@ fun () -> Eqaf.equal hash_neq_0 hash_neq_1)

let cfg = Benchmark.cfg ~start:100

let test_compare0 =
  Test.make ~name:"equal"
    (Staged.stage @@ fun () -> Eqaf.compare_be hash_eq_0 hash_eq_1)
let test_compare1 =
  Test.make ~name:"not equal"
    (Staged.stage @@ fun () -> Eqaf.compare_be hash_neq_0 hash_neq_1)

let f_eq_0 (v : int) = v = Char.code chr_into_hash_eq_0
let f_neq_0 (v : int) = v = Char.code random_chr

let test_exists0 =
  Test.make ~name:"equal"
    (Staged.stage @@ fun () -> Eqaf.exists_uint8 ~f:f_eq_0 hash_eq_0)
let test_exists1 =
  Test.make ~name:"not equal"
    (Staged.stage @@ fun () -> Eqaf.exists_uint8 ~f:f_neq_0 hash_neq_0)

let f_hash_eq_0 (v : int) = v = Char.code chr_into_hash_eq_0
let f_random (v : int) = v = Char.code random_chr

let test_find0 =
  Test.make ~name:"equal"
    (Staged.stage @@ fun () -> Eqaf.find_uint8 ~f:f_hash_eq_0 hash_eq_0)
let test_find1 =
  Test.make ~name:"not equal"
    (Staged.stage @@ fun () -> Eqaf.find_uint8 ~f:f_random hash_neq_0)

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ monotonic_clock ]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~stabilize:true ~quota:(Time.second 1.)
      ~start:1000 ~kde:(Some 1000) ()
  in
  let test_equal =
    Test.make_grouped ~name:"equal" ~fmt:"%s %s"
      [ test_equal0; test_equal1 ]
  in
  let test_compare =
    Test.make_grouped ~name:"compare" ~fmt:"%s %s"
      [ test_compare0; test_compare1 ]
  in
  let test_exists =
    Test.make_grouped ~name:"exists" ~fmt:"%s %s"
      [ test_exists0; test_exists1 ]
  in
  let test_find =
    Test.make_grouped ~name:"find" ~fmt:"%s %s"
      [ test_find0; test_find1 ]
  in
  let raw_results =
    Benchmark.all cfg instances
      (Test.make_grouped ~name:"benchmark" ~fmt:"%s %s"
         [ test_equal; test_compare; test_exists; test_find ])
  in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let pr_bench name value =
    Format.printf
      {|{"results": [{"name": "eqaf", "metrics": [{"name": "%s", "value": %f, "units": "ns"}]}]}@.|}
      name value
  in
  let results = Analyze.merge ols instances results in
  let timings = Hashtbl.find results "monotonic-clock" in
  Hashtbl.iter
    (fun c v ->
       match Analyze.OLS.estimates v with
       | None -> ()
       | Some ts -> List.iter (pr_bench c) ts)
    timings;
  ()

let () = benchmark ()
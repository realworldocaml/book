open Core
open Core_bench
module Entry = Ppx_bench_lib.Benchmark_accumulator.Entry

let make_benchmark_name entry =
  let module_name =
    match Entry.get_module_name_opt entry with
    | Some s -> ":" ^ s
    | None -> ""
  in
  let bench_module_name =
    match entry.Entry.bench_module_name with
    | Some s -> ":" ^ s
    | None -> ""
  in
  String.concat
    [ "["; entry.Entry.filename; module_name; bench_module_name; "] "; entry.Entry.name ]
;;

(* Code for filtering the out the benchmarks to run *)
let entry_to_bench_test entry ~key =
  let open Entry in
  let name = make_benchmark_name entry in
  let test_name = entry.name in
  let file_name = entry.filename in
  let module_name = entry.bench_module_name in
  match entry.Entry.test_spec with
  | Regular_thunk f ->
    Bench.Test.create_with_initialization ~name ~test_name ~file_name ?module_name ~key f
  | Parameterised_thunk { params; thunk; _ } ->
    Bench.Test.create_parameterised
      ~name
      ~test_name
      ~file_name
      ?module_name
      ~args:params
      ~key
      (fun len -> Staged.stage (thunk len))
;;

let pattern_to_predicate s =
  let regexp = Re.Perl.compile_pat s in
  fun name -> Re.execp regexp name
;;

let get_matching_tests ~libname patterns =
  let tbl = Int.Table.create () in
  let entries = Ppx_bench_lib.Benchmark_accumulator.lookup_lib ~libname in
  let entries =
    match patterns with
    (* if no regexes are specified, run all entries *)
    | [] -> entries
    | _ :: _ ->
      let filter =
        let preds = List.map patterns ~f:pattern_to_predicate in
        fun name -> List.exists preds ~f:(fun pred -> pred name)
      in
      (* for parameterized tests we must include the param in the filter (so we can filter
         to "size:1000" or what have you.) *)
      List.filter_map entries ~f:(fun entry ->
        let name = make_benchmark_name entry in
        match entry.Entry.test_spec with
        | Regular_thunk _ -> Option.some_if (filter name) entry
        | Parameterised_thunk { params; arg_name; thunk } ->
          let params =
            List.filter params ~f:(fun (p, _) ->
              let name = name ^ ":" ^ p in
              filter name)
          in
          (match params with
           | [] -> None
           | _ :: _ ->
             Some
               (Entry.with_test_spec
                  entry
                  (Parameterised_thunk { params; arg_name; thunk }))))
  in
  let tests =
    List.map entries ~f:(fun entry ->
      let key = entry.Entry.unique_id in
      Hashtbl.add_exn tbl ~key ~data:entry;
      entry_to_bench_test entry ~key)
  in
  tbl, tests
;;

let get_matching_tests_no_dups ~libname patterns =
  let _tbl, tests = get_matching_tests ~libname patterns in
  let tests =
    List.dedup_and_sort tests ~compare:(fun t1 t2 ->
      String.compare (Bench.Test.name t1) (Bench.Test.name t2))
  in
  tests
;;

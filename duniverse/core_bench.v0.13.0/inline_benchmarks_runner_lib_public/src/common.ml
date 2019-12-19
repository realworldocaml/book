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
    [ "["
    ; entry.Entry.filename
    ; module_name
    ; bench_module_name
    ; "] "
    ; entry.Entry.name
    ]

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
  | Indexed_thunk { arg_values; thunk; _ } ->
    Bench.Test.create_indexed
      ~name ~test_name ~file_name ?module_name ~args:arg_values ~key
      (fun len -> Staged.stage (thunk len))

let pattern_to_predicate s =
  let regexp = Re.Perl.compile_pat s in
  (fun entry ->
     let name = make_benchmark_name entry in
     Re.execp regexp name)

let get_matching_tests ~libname patterns =
  let tbl = Int.Table.create () in
  let entries = Ppx_bench_lib.Benchmark_accumulator.lookup_lib ~libname in
  let entries =
    match patterns with
    (* if no regexes are specified not specified, run all entries *)
    | [] -> entries
    | _ :: _ ->
      List.dedup_and_sort ~compare:Entry.compare
        (List.concat_map patterns ~f:(fun pattern ->
           let entries = List.filter entries ~f:(pattern_to_predicate pattern) in
           if List.is_empty entries
           then printf "Warning: %s didn't match any benchmark\n" pattern;
           entries))
  in
  let tests =
    List.map entries ~f:(fun entry ->
      let key = entry.Entry.unique_id in
      Hashtbl.add_exn tbl ~key ~data:entry;
      entry_to_bench_test entry ~key)
  in
  tbl, tests

let get_matching_tests_no_dups ~libname patterns =
  let _tbl, tests = get_matching_tests ~libname patterns in
  let tests = List.dedup_and_sort tests ~compare:(fun t1 t2 ->
    String.compare (Bench.Test.name t1) (Bench.Test.name t2))
  in
  tests


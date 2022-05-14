open! Base

let%bench_module "Overheads" =
  (module struct
    (* Using [%bench_fun] to bind the input outside the benchmarked code actually has less
       overhead then using [%bench] naively. *)
    let%bench_fun "int overhead" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Fn.id n
    ;;

    let%bench_fun "int64 overhead" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Fn.id n
    ;;

    let%bench_fun "int32 overhead" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Fn.id n
    ;;

    let%bench_fun "nativeint overhead" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Fn.id n
    ;;
  end)
;;

let%bench_module "Clz" =
  (module struct
    (* ocaml_intrinsics library *)
    let%bench_fun "int_clz" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Ocaml_intrinsics.Int.count_leading_zeros n
    ;;

    let%bench_fun "int_clz2" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Ocaml_intrinsics.Int.count_leading_zeros2 n
    ;;

    let%bench_fun "int64_clz" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Ocaml_intrinsics.Int64.count_leading_zeros n
    ;;

    let%bench_fun "nativeint_clz" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Ocaml_intrinsics.Nativeint.count_leading_zeros n
    ;;

    let%bench_fun "int32_clz" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Ocaml_intrinsics.Int32.count_leading_zeros n
    ;;

    (* Base *)
    let%bench_fun "base int_clz" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Base.Int.clz n
    ;;

    let%bench_fun "base int64_clz" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Base.Int64.clz n
    ;;

    let%bench_fun "base nativeint_clz" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Base.Nativeint.clz n
    ;;

    let%bench_fun "base int32_clz" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Base.Int32.clz n
    ;;
  end)
;;

let%bench_module "Ctz" =
  (module struct
    (* ocaml_intrinsics library *)
    let%bench_fun "int_ctz" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Ocaml_intrinsics.Int.count_trailing_zeros n
    ;;

    let%bench_fun "int64_ctz" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Ocaml_intrinsics.Int64.count_trailing_zeros n
    ;;

    let%bench_fun "nativeint_ctz" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Ocaml_intrinsics.Nativeint.count_trailing_zeros n
    ;;

    let%bench_fun "int32_ctz" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Ocaml_intrinsics.Int32.count_trailing_zeros n
    ;;

    (* Base *)
    let%bench_fun "base int_ctz" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Base.Int.ctz n
    ;;

    let%bench_fun "base int64_ctz" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Base.Int64.ctz n
    ;;

    let%bench_fun "base nativeint_ctz" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Base.Nativeint.ctz n
    ;;

    let%bench_fun "base int32_ctz" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Base.Int32.ctz n
    ;;
  end)
;;

let%bench_module "Popcnt" =
  (module struct
    (* ocaml_intrinsics library *)
    let%bench_fun "int_popcount" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Ocaml_intrinsics.Int.count_set_bits n
    ;;

    let%bench_fun "int_popcount2" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Ocaml_intrinsics.Int.count_set_bits2 n
    ;;

    let%bench_fun "int64_popcount" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Ocaml_intrinsics.Int64.count_set_bits n
    ;;

    let%bench_fun "nativeint_popcount" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Ocaml_intrinsics.Nativeint.count_set_bits n
    ;;

    let%bench_fun "int32_popcount" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Ocaml_intrinsics.Int32.count_set_bits n
    ;;

    (* Base  *)
    let%bench_fun "base int_popcount" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Base.Int.popcount n
    ;;

    let%bench_fun "base int64_popcount" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Base.Int64.popcount n
    ;;

    let%bench_fun "base nativeint_popcount" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Base.Nativeint.popcount n
    ;;

    let%bench_fun "base int32_popcount" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Base.Int32.popcount n
    ;;
  end)
;;

let%bench_module "Perfmon.rdtsc" =
  (module struct
    (* ocaml_intrinsics library *)
    let%bench_fun "rdtsc" = fun () -> Ocaml_intrinsics.Perfmon.rdtsc ()

    (* ocaml_intrinsics library *)
    let%bench_fun "int_of_rdtsc" =
      fun () -> Int64.to_int_trunc (Ocaml_intrinsics.Perfmon.rdtsc ())
    ;;

    (* Time_stamp_counter *)
    let%bench_fun "Time_stamp_counter.now" = fun () -> Time_stamp_counter.now ()
  end)
;;

let%bench_module "Perfmon.rdpmc" =
  (module struct
    let cpu_count_hw_cpu_cycles = (1 lsl 30) + 1

    let%bench_fun "rdpmc" =
      fun () -> Ocaml_intrinsics.Perfmon.rdpmc (Int32.of_int_exn cpu_count_hw_cpu_cycles)
    ;;
  end)
;;

let%bench_module "Crc" =
  (module struct
    (* ocaml_intrinsics library *)

    let%bench_fun "int_crc" =
      let initial = Sys.opaque_identity (Random.int Int.max_value) in
      let data = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Ocaml_intrinsics.Crc.int_crc ~initial ~data
    ;;

    let%bench_fun "int64_crc" =
      let initial = Sys.opaque_identity (Random.int Int.max_value) in
      let data = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Ocaml_intrinsics.Crc.int64_crc ~initial ~data
    ;;

    let%bench_fun "iterated_crc_exn" =
      let initial = Sys.opaque_identity (Random.int Int.max_value) in
      let data = Sys.opaque_identity (Random.int Int.max_value) in
      let iterations = Sys.opaque_identity 100 in
      fun () -> Ocaml_intrinsics.Crc.iterated_crc_exn ~initial ~data ~iterations
    ;;
  end)
;;

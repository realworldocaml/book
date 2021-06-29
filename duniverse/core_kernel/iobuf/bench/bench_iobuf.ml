open! Core_kernel
open! Iobuf

(* Minimal blit benchmarks. *)
(* ┌────────────────────────────────────────┬────────────┬────────────┐
 * │ Name                                   │   Time/Run │ Percentage │
 * ├────────────────────────────────────────┼────────────┼────────────┤
 * │ [iobuf.ml:Blit] string:5               │    15.30ns │      1.11% │
 * │ [iobuf.ml:Blit] string:10              │    15.57ns │      1.13% │
 * │ [iobuf.ml:Blit] string:100             │    19.26ns │      1.39% │
 * │ [iobuf.ml:Blit] string:1000            │    47.83ns │      3.46% │
 * │ [iobuf.ml:Blit] string:10000           │   197.90ns │     14.32% │
 * │ [iobuf.ml:Blit] blito:5                │    24.38ns │      1.76% │
 * │ [iobuf.ml:Blit] blito:10               │    26.88ns │      1.94% │
 * │ [iobuf.ml:Blit] blito:100              │    30.01ns │      2.17% │
 * │ [iobuf.ml:Blit] blito:1000             │    57.83ns │      4.18% │
 * │ [iobuf.ml:Blit] blito:10000            │   391.42ns │     28.31% │
 * │ [iobuf.ml:Blit] consume:5              │    23.00ns │      1.66% │
 * │ [iobuf.ml:Blit] consume:10             │    25.36ns │      1.83% │
 * │ [iobuf.ml:Blit] consume:100            │    29.79ns │      2.15% │
 * │ [iobuf.ml:Blit] consume:1000           │    58.93ns │      4.26% │
 * │ [iobuf.ml:Blit] consume:10000          │   395.19ns │     28.59% │
 * │ [iobuf.ml:Blit] fill:5                 │    24.28ns │      1.76% │
 * │ [iobuf.ml:Blit] fill:10                │    26.84ns │      1.94% │
 * │ [iobuf.ml:Blit] fill:100               │    29.54ns │      2.14% │
 * │ [iobuf.ml:Blit] fill:1000              │    57.05ns │      4.13% │
 * │ [iobuf.ml:Blit] fill:10000             │   395.72ns │     28.62% │
 * │ [iobuf.ml:Blit] consume_and_fill:5     │    25.43ns │      1.84% │
 * │ [iobuf.ml:Blit] consume_and_fill:10    │    27.19ns │      1.97% │
 * │ [iobuf.ml:Blit] consume_and_fill:100   │    30.96ns │      2.24% │
 * │ [iobuf.ml:Blit] consume_and_fill:1000  │    58.38ns │      4.22% │
 * │ [iobuf.ml:Blit] consume_and_fill:10000 │   383.62ns │     27.75% │
 * │ [iobuf.ml:Blit] unsafe [overlap]:5     │    14.25ns │      1.03% │
 * │ [iobuf.ml:Blit] unsafe [overlap]:10    │    16.92ns │      1.23% │
 * │ [iobuf.ml:Blit] unsafe [overlap]:100   │    37.17ns │      2.70% │
 * │ [iobuf.ml:Blit] unsafe [overlap]:1000  │   169.60ns │     12.32% │
 * │ [iobuf.ml:Blit] unsafe [overlap]:10000 │ 1_377.01ns │    100.00% │
 * └────────────────────────────────────────┴────────────┴────────────┘ *)
let%bench_module "Blit" =
  (module struct
    let lengths = [ 5; 10; 100; 1000; 10_000 ]

    let%bench_fun ("string"[@indexed len = lengths]) =
      let buf = create ~len in
      let str = Bytes.create len in
      fun () -> Peek.To_bytes.blit ~src:buf ~dst:str ~src_pos:0 ~dst_pos:0 ~len
    ;;

    let%bench_fun ("blito"[@indexed len = lengths]) =
      let src = create ~len in
      let dst = create ~len in
      fun () -> Blit.blito () ~src ~dst
    ;;

    let%bench_fun ("consume"[@indexed len = lengths]) =
      let src = create ~len in
      let dst = create ~len in
      fun () ->
        Blit_consume.blito () ~src ~dst;
        reset src
    ;;

    let%bench_fun ("fill"[@indexed len = lengths]) =
      let src = create ~len in
      let dst = create ~len in
      fun () ->
        Blit_fill.blito () ~src ~dst;
        reset dst
    ;;

    let%bench_fun ("consume_and_fill"[@indexed len = lengths]) =
      let src = create ~len in
      let dst = create ~len in
      fun () ->
        Blit_consume_and_fill.blito () ~src ~dst;
        reset src;
        reset dst
    ;;

    let%bench_fun ("unsafe [overlap]"[@indexed len = lengths]) =
      let t = create ~len:(len + 1) in
      fun () -> Blit.unsafe_blit ~src:t ~dst:t ~len ~src_pos:0 ~dst_pos:1
    ;;
  end)
;;


(* 8b4516d0ab0a *"jane/int-repr/iobuf-bench/update-columns"
   Estimated testing time 20s (20 benchmarks x 1s). Change using '-quota'.
   ┌───────────────────────────────────────┬────────────────┬──────────┐
   │ Name                                  │ Runs @ Samples │ Time/Run │
   ├───────────────────────────────────────┼────────────────┼──────────┤
   │ [bench_iobuf.ml:Poke] char            │ 2967848 @ 1192 │   3.34ns │
   │ [bench_iobuf.ml:Poke] uint8_trunc     │ 2938464 @ 1191 │   3.40ns │
   │ [bench_iobuf.ml:Poke] int8_trunc      │ 2909371 @ 1190 │   3.43ns │
   │ [bench_iobuf.ml:Poke] int16_be_trunc  │ 2713626 @ 1183 │   3.65ns │
   │ [bench_iobuf.ml:Poke] int16_le_trunc  │ 2581924 @ 1178 │   3.86ns │
   │ [bench_iobuf.ml:Poke] uint16_be_trunc │ 2713626 @ 1183 │   3.66ns │
   │ [bench_iobuf.ml:Poke] uint16_le_trunc │ 2909371 @ 1190 │   3.42ns │
   │ [bench_iobuf.ml:Poke] int32_be_trunc  │ 2713626 @ 1183 │   3.69ns │
   │ [bench_iobuf.ml:Poke] int32_le_trunc  │ 2660158 @ 1181 │   3.70ns │
   │ [bench_iobuf.ml:Poke] uint32_be_trunc │ 2740762 @ 1184 │   3.63ns │
   │ [bench_iobuf.ml:Poke] uint32_le_trunc │ 2852046 @ 1188 │   3.49ns │
   │ [bench_iobuf.ml:Poke] int64_be        │ 2880566 @ 1189 │   3.46ns │
   │ [bench_iobuf.ml:Poke] int64_le:4      │ 3181932 @ 1199 │   3.13ns │
   │ [bench_iobuf.ml:Poke] int64_le:12     │ 3119236 @ 1197 │   3.17ns │
   │ [bench_iobuf.ml:Poke] int64_le:20     │ 3181932 @ 1199 │   3.11ns │
   │ [bench_iobuf.ml:Poke] int64_le:28     │ 3181932 @ 1199 │   3.12ns │
   │ [bench_iobuf.ml:Poke] int64_le:36     │ 3150428 @ 1198 │   3.13ns │
   │ [bench_iobuf.ml:Poke] int64_le:44     │ 3150428 @ 1198 │   3.13ns │
   │ [bench_iobuf.ml:Poke] int64_le:52     │ 3119236 @ 1197 │   3.17ns │
   │ [bench_iobuf.ml:Poke] int64_le:60     │ 3150428 @ 1198 │   3.14ns │
   └───────────────────────────────────────┴────────────────┴──────────┘ *)
let%bench_module "Poke" =
  (module struct
    let iobuf = of_string (String.make 72 '\000') (* cache line + word size *)

    let pos = Sys.opaque_identity 0

    let%bench "char" = Poke.char iobuf ~pos (Sys.opaque_identity 'a')
    let%bench "uint8_trunc" = Poke.uint8_trunc iobuf ~pos (Sys.opaque_identity pos)
    let%bench "int8_trunc" = Poke.int8_trunc iobuf ~pos (Sys.opaque_identity pos)
    let%bench "int16_be_trunc" = Poke.int16_be_trunc iobuf ~pos (Sys.opaque_identity pos)
    let%bench "int16_le_trunc" = Poke.int16_le_trunc iobuf ~pos (Sys.opaque_identity pos)

    let%bench "uint16_be_trunc" =
      Poke.uint16_be_trunc iobuf ~pos (Sys.opaque_identity pos)
    ;;

    let%bench "uint16_le_trunc" =
      Poke.uint16_le_trunc iobuf ~pos (Sys.opaque_identity pos)
    ;;

    let%bench "int32_be_trunc" = Poke.int32_be_trunc iobuf ~pos (Sys.opaque_identity pos)
    let%bench "int32_le_trunc" = Poke.int32_le_trunc iobuf ~pos (Sys.opaque_identity pos)

    let%bench "uint32_be_trunc" =
      Poke.uint32_be_trunc iobuf ~pos (Sys.opaque_identity pos)
    ;;

    let%bench "uint32_le_trunc" =
      Poke.uint32_le_trunc iobuf ~pos (Sys.opaque_identity pos)
    ;;

    let%bench "int64_be" = Poke.int64_be iobuf ~pos (Sys.opaque_identity pos)

    (* We test a few offsets to see if alignment affects performance. *)
    let%bench_fun ("int64_le"[@indexed pos = [ 4; 12; 20; 28; 36; 44; 52; 60 ]]) =
      fun () -> Poke.int64_le iobuf ~pos (Sys.opaque_identity pos)
    ;;
  end)
;;

(* 8b4516d0ab0a *"jane/int-repr/iobuf-bench/update-columns"
   Estimated testing time 22s (22 benchmarks x 1s). Change using '-quota'.
   ┌─────────────────────────────────────────┬────────────────┬──────────┐
   │ Name                                    │ Runs @ Samples │ Time/Run │
   ├─────────────────────────────────────────┼────────────────┼──────────┤
   │ [bench_iobuf.ml:Peek] char              │ 3088353 @ 1196 │   3.22ns │
   │ [bench_iobuf.ml:Peek] uint8             │ 3088353 @ 1196 │   3.22ns │
   │ [bench_iobuf.ml:Peek] int8              │ 3088353 @ 1196 │   3.24ns │
   │ [bench_iobuf.ml:Peek] int16_be          │ 2633820 @ 1180 │   3.77ns │
   │ [bench_iobuf.ml:Peek] int16_le          │ 2337388 @ 1168 │   4.24ns │
   │ [bench_iobuf.ml:Peek] uint16_be         │ 2938464 @ 1191 │   3.37ns │
   │ [bench_iobuf.ml:Peek] uint16_le         │ 3088353 @ 1196 │   3.23ns │
   │ [bench_iobuf.ml:Peek] int32_be          │ 2967848 @ 1192 │   3.33ns │
   │ [bench_iobuf.ml:Peek] int32_le          │ 3057776 @ 1195 │   3.25ns │
   │ [bench_iobuf.ml:Peek] uint32_be         │ 2938464 @ 1191 │   3.37ns │
   │ [bench_iobuf.ml:Peek] uint32_le         │ 2997526 @ 1193 │   3.31ns │
   │ [bench_iobuf.ml:Peek] int64_be_exn      │ 2967848 @ 1192 │   3.33ns │
   │ [bench_iobuf.ml:Peek] int64_le_exn      │ 2997526 @ 1193 │   3.30ns │
   │ [bench_iobuf.ml:Peek] int64_be_trunc    │ 3027501 @ 1194 │   3.28ns │
   │ [bench_iobuf.ml:Peek] int64_le_trunc:4  │ 3377682 @ 1205 │   2.92ns │
   │ [bench_iobuf.ml:Peek] int64_le_trunc:12 │ 3377682 @ 1205 │   2.93ns │
   │ [bench_iobuf.ml:Peek] int64_le_trunc:20 │ 3377682 @ 1205 │   2.91ns │
   │ [bench_iobuf.ml:Peek] int64_le_trunc:28 │ 3377682 @ 1205 │   2.92ns │
   │ [bench_iobuf.ml:Peek] int64_le_trunc:36 │ 3377682 @ 1205 │   2.91ns │
   │ [bench_iobuf.ml:Peek] int64_le_trunc:44 │ 3377682 @ 1205 │   2.92ns │
   │ [bench_iobuf.ml:Peek] int64_le_trunc:52 │ 3377682 @ 1205 │   2.92ns │
   │ [bench_iobuf.ml:Peek] int64_le_trunc:60 │ 3311129 @ 1203 │   2.98ns │
   └─────────────────────────────────────────┴────────────────┴──────────┘ *)
let%bench_module "Peek" =
  (module struct
    let iobuf = of_string (String.make 72 '\000') (* cache line + word size *)

    let pos = Sys.opaque_identity 0

    let%bench "char" = Peek.char iobuf ~pos
    let%bench "uint8" = Peek.uint8 iobuf ~pos
    let%bench "int8" = Peek.int8 iobuf ~pos
    let%bench "int16_be" = Peek.int16_be iobuf ~pos
    let%bench "int16_le" = Peek.int16_le iobuf ~pos
    let%bench "uint16_be" = Peek.uint16_be iobuf ~pos
    let%bench "uint16_le" = Peek.uint16_le iobuf ~pos
    let%bench "int32_be" = Peek.int32_be iobuf ~pos
    let%bench "int32_le" = Peek.int32_le iobuf ~pos
    let%bench "uint32_be" = Peek.uint32_be iobuf ~pos
    let%bench "uint32_le" = Peek.uint32_le iobuf ~pos
    let%bench "int64_be_exn" = Peek.int64_be_exn iobuf ~pos
    let%bench "int64_le_exn" = Peek.int64_le_exn iobuf ~pos
    let%bench "int64_be_trunc" = Peek.int64_be_trunc iobuf ~pos

    (* We test a few offsets to see if alignment affects performance. *)
    let%bench_fun ("int64_le_trunc"[@indexed pos = [ 4; 12; 20; 28; 36; 44; 52; 60 ]]) =
      fun () -> Peek.int64_le_trunc iobuf ~pos
    ;;
  end)
;;

let%bench_module "decimal" =
  (module struct
    (* Quantify the gain from our version of [Fill.decimal] over [Int.to_string]. *)
    let values =
      [ Int.min_value
      ; Int.min_value + 1
      ; -10_000
      ; 0
      ; 35
      ; 1_000
      ; 1_000_000
      ; Int.max_value
      ]
    ;;

    let iobuf = create ~len:32

    let%bench_fun ("Fill"[@indexed x = values]) =
      fun () ->
      reset iobuf;
      Fill.decimal iobuf x
    ;;

    let%bench_fun ("Unsafe.Fill"[@indexed x = values]) =
      fun () ->
        reset iobuf;
        Unsafe.Fill.decimal iobuf x
    ;;

    let%bench_fun ("Fill.stringo"[@indexed x = values]) =
      fun () ->
        reset iobuf;
        Fill.stringo iobuf (Int.to_string x)
    ;;
  end)
;;

(* In an attempt to verify how much a phys_equal check could optimize
   [set_bounds_and_buffer], in the soon-to-be-common protogen use case, here is the result
   with cross-module inlining:


   ┌────────────────────────────────────────────────────────┬──────────┬─────────┬────────────┐
   │ Name                                                   │ Time/Run │ mWd/Run │ Percentage │
   ├────────────────────────────────────────────────────────┼──────────┼─────────┼────────────┤
   │ [iobuf.ml:set_bounds_and_buffer] with-write-barrier    │   7.78ns │         │    100.00% │
   │ [iobuf.ml:set_bounds_and_buffer] without-write-barrier │   3.92ns │         │     50.40% │
   │ [iobuf.ml:set_bounds_and_buffer] with-gc               │   5.34ns │   6.00w │     68.65% │
   └────────────────────────────────────────────────────────┴──────────┴─────────┴────────────┘

   and here is the code for the benchmark:

   {[
     let orig_set_bounds_and_buffer ~src ~dst =
       dst.lo_min <- src.lo_min;
       dst.lo <- src.lo;
       dst.hi <- src.hi;
       dst.hi_max <- src.hi_max;
       dst.buf <- src.buf
     ;;

     let new_set_bounds_and_buffer = set_bounds_and_buffer

     let%bench_module "set_bounds_and_buffer" = (module struct
       let src = create ~len:32
       let dst = create ~len:32

       let%bench_fun "with-write-barrier" =
         (fun () -> orig_set_bounds_and_buffer ~src ~dst)
       let%bench_fun "without-write-barrier" =
         (fun () -> new_set_bounds_and_buffer ~src ~dst)
       let%bench_fun "with-gc" =
         (fun () -> sub_shared src)
     end) ]} *)

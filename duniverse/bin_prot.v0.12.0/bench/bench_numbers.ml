open Bin_prot

(* These benchmarks are not supposed to show the performances of individual functions. For
   instance some bench run the benched function more times than other.

   These benchmarks are only intended to check performances regression. When one changes
   bin_prot, they should check that the results of these is not worse.
*)

let%bench_module "write numbers" =
  (module struct
    open Write

    let buf = Common.create_buf 64

    let ign (_ : int) = ()

    let%bench "bin_write_char" =
      for i = 0 to 255 do
        ign @@ bin_write_char buf ~pos:0 (Char.unsafe_chr i)
      done

    let%bench "bin_write_int" =
      for bit = 0 to Sys.word_size - 2 do
        let n = 1 lsl bit in
        ign @@ bin_write_int buf ~pos:0 n;
        ign @@ bin_write_int buf ~pos:0 (-n)
      done

    let%bench "bin_write_nat0" =
      for bit = 0 to Sys.word_size - 2 do
        let n = Nat0.unsafe_of_int (1 lsl bit) in
        ign @@ bin_write_nat0 buf ~pos:0 n
      done

    let%bench "bin_write_float" =
      for i = 0 to 10 do
        ign @@ bin_write_float buf ~pos:0 (float i)
      done

    let%bench "bin_write_int32" =
      for bit = 0 to 31 do
        ign @@ bin_write_int32 buf ~pos:0 (Int32.shift_left 1l bit);
        ign @@ bin_write_int32 buf ~pos:0 (Int32.neg (Int32.shift_left 1l bit));
      done

    let%bench "bin_write_int64" =
      for bit = 0 to 63 do
        ign @@ bin_write_int64 buf ~pos:0 (Int64.shift_left 1L bit);
        ign @@ bin_write_int64 buf ~pos:0 (Int64.neg (Int64.shift_left 1L bit));
      done

    let%bench "bin_write_nativeint" =
      for bit = 0 to Sys.word_size - 1 do
        ign @@ bin_write_nativeint buf ~pos:0 (Nativeint.shift_left 1n bit);
        ign @@ bin_write_nativeint buf ~pos:0 (Nativeint.neg (Nativeint.shift_left 1n bit));
      done

    let%bench "bin_write_variant_int" =
      for bit = 0 to Sys.word_size - 1 do
        ign @@ bin_write_variant_int buf ~pos:0 (1 lsl bit)
      done

    let%bench "bin_write_int_8bit" =
      for bit = 0 to 7 do
        ign @@ bin_write_int_8bit buf ~pos:0 (1 lsl bit)
      done

    let%bench "bin_write_int_16bit" =
      for bit = 0 to 15 do
        ign @@ bin_write_int_16bit buf ~pos:0 (1 lsl bit)
      done

    let%bench "bin_write_int_32bit" =
      for bit = 0 to 31 do
        ign @@ bin_write_int_32bit buf ~pos:0 (1 lsl bit)
      done

    let%bench "bin_write_int_64bit" =
      for bit = 0 to 63 do
        ign @@ bin_write_int_64bit buf ~pos:0 (1 lsl bit)
      done

    let%bench "bin_write_int64_bits" =
      for bit = 0 to 63 do
        ign @@ bin_write_int64_bits buf ~pos:0 (Int64.shift_left 1L bit)
      done

    let%bench "bin_write_network16_int" =
      for bit = 0 to 15 do
        ign @@ bin_write_network16_int buf ~pos:0 (1 lsl bit)
      done

    let%bench "bin_write_network32_int" =
      for bit = 0 to 31 do
        ign @@ bin_write_network32_int buf ~pos:0 (1 lsl bit)
      done

    let%bench "bin_write_network64_int" =
      for bit = 0 to 63 do
        ign @@ bin_write_network64_int buf ~pos:0 (1 lsl bit)
      done

    let%bench "bin_write_network32_int32" =
      for bit = 0 to 31 do
        ign @@ bin_write_network32_int32 buf ~pos:0 (Int32.shift_left 1l bit)
      done

    let%bench "bin_write_network64_int64" =
      for bit = 0 to 63 do
        ign @@ bin_write_network64_int64 buf ~pos:0 (Int64.shift_left 1L bit)
      done
  end)

let%bench_module "write+read numbers" =
  (module struct
    open Read
    open Write

    let buf = Common.create_buf 64

    let ign (_ : int) = ()
    let ign_char (_ : char) = ()
    let ign_nat0 (_ : Nat0.t) = ()
    let ign_int (_ : int) = ()
    let ign_float f = ignore (truncate f : int)

    let pos_ref = ref 0

    let%bench "bin_read_char" =
      for i = 0 to 255 do
        ign @@ bin_write_char buf ~pos:0 (Char.unsafe_chr i);
        bin_read_char buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_char
      done

    let%bench "bin_read_int" =
      for bit = 0 to Sys.word_size - 2 do
        let n = 1 lsl bit in
        ign @@ bin_write_int buf ~pos:0 n;
        bin_read_int buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_int;
        ign @@ bin_write_int buf ~pos:0 (-n);
        bin_read_int buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_int;
      done

    let%bench "bin_read_nat0" =
      for bit = 0 to Sys.word_size - 2 do
        let n = Nat0.unsafe_of_int (1 lsl bit) in
        ign @@ bin_write_nat0 buf ~pos:0 n;
        bin_read_nat0 buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_nat0;
      done

    let%bench "bin_read_float" =
      for i = 0 to 10 do
        ign @@ bin_write_float buf ~pos:0 (float i);
        bin_read_float buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_float;
      done

    let%bench "bin_read_int32" =
      for bit = 0 to 31 do
        ign @@ bin_write_int32 buf ~pos:0 (Int32.shift_left 1l bit);
        bin_read_int32 buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> Int32.to_int |> ign_int;
        ign @@ bin_write_int32 buf ~pos:0 (Int32.neg (Int32.shift_left 1l bit));
        bin_read_int32 buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> Int32.to_int |> ign_int;
      done

    let%bench "bin_read_int64" =
      for bit = 0 to 63 do
        ign @@ bin_write_int64 buf ~pos:0 (Int64.shift_left 1L bit);
        bin_read_int64 buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> Int64.to_int |> ign_int;
        ign @@ bin_write_int64 buf ~pos:0 (Int64.neg (Int64.shift_left 1L bit));
        bin_read_int64 buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> Int64.to_int |> ign_int;
      done

    let%bench "bin_read_nativeint" =
      for bit = 0 to Sys.word_size - 1 do
        ign @@ bin_write_nativeint buf ~pos:0 (Nativeint.shift_left 1n bit);
        bin_read_nativeint buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> Nativeint.to_int |> ign_int;
        ign @@ bin_write_nativeint buf ~pos:0 (Nativeint.neg (Nativeint.shift_left 1n bit));
        bin_read_nativeint buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> Nativeint.to_int |> ign_int;
      done

    let%bench "bin_read_variant_int" =
      for bit = 0 to Sys.word_size - 1 do
        ign @@ bin_write_variant_int buf ~pos:0 (1 lsl bit);
        bin_read_variant_int buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_int;
      done

    let%bench "bin_read_int_8bit" =
      for bit = 0 to 7 do
        ign @@ bin_write_int_8bit buf ~pos:0 (1 lsl bit);
        bin_read_int_8bit buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_int;
      done

    let%bench "bin_read_int_16bit" =
      for bit = 0 to 15 do
        ign @@ bin_write_int_16bit buf ~pos:0 (1 lsl bit);
        bin_read_int_16bit buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_int
      done

    let%bench "bin_read_int_32bit" =
      for bit = 0 to 31 do
        ign @@ bin_write_int_32bit buf ~pos:0 (1 lsl bit);
        bin_read_int_32bit buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_int
      done

    let%bench "bin_read_int_64bit" =
      for bit = 0 to 63 do
        ign @@ bin_write_int_64bit buf ~pos:0 (1 lsl bit);
        bin_read_int_64bit buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_int
      done

    let%bench "bin_read_int64_bits" =
      for bit = 0 to 63 do
        ign @@ bin_write_int64_bits buf ~pos:0 (Int64.shift_left 1L bit);
        bin_read_int64_bits buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> Int64.to_int |> ign_int
      done

    let%bench "bin_read_network16_int" =
      for bit = 0 to 15 do
        ign @@ bin_write_network16_int buf ~pos:0 (1 lsl bit);
        bin_read_network16_int buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_int
      done

    let%bench "bin_read_network32_int" =
      for bit = 0 to 31 do
        ign @@ bin_write_network32_int buf ~pos:0 (1 lsl bit);
        bin_read_network32_int buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_int
      done

    let%bench "bin_read_network64_int" =
      for bit = 0 to 63 do
        ign @@ bin_write_network64_int buf ~pos:0 (1 lsl bit);
        bin_read_network64_int buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> ign_int
      done

    let%bench "bin_read_network32_int32" =
      for bit = 0 to 31 do
        ign @@ bin_write_network32_int32 buf ~pos:0 (Int32.shift_left 1l bit);
        bin_read_network32_int32 buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> Int32.to_int |> ign_int
      done

    let%bench "bin_read_network64_int64" =
      for bit = 0 to 63 do
        ign @@ bin_write_network64_int64 buf ~pos:0 (Int64.shift_left 1L bit);
        bin_read_network64_int64 buf ~pos_ref:(pos_ref := 0; pos_ref)
        |> Int64.to_int |> ign_int
      done
  end)

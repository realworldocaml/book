BENCH_FUN "Array.get (tuple)" =
  (* This is mis-indented only when BENCH_FUN is on the first line. *)
  let len = 300 in
  let arr = create ~len (1,2) in
  (fun () -> ignore(arr.(len-1)))

BENCH_FUN "Array.set (tuple)" =
  let len = 300 in
  let arr = create ~len (1,2) in
  (fun () -> arr.(len-1) <- (3,4))

(* Some benchmarks of the blit operations *)
BENCH_MODULE "Blit tests" = struct
  let lengths = [0; 10; 100; 1000; 10_000]

  BENCH_MODULE "Int" = struct
    BENCH_INDEXED "blit" len lengths  =
      let src = create ~len 0 in
      let dst = create ~len 0 in
      (fun () -> Int.blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len)

    BENCH_INDEXED "blito" len lengths  =
      let src = create ~len 0 in
      let dst = create ~len 0 in
      (fun () -> Int.blito ~src ~src_pos:0 ~dst ~dst_pos:0 ~src_len:len ())
  end

  BENCH_MODULE "Float" = struct
    BENCH_INDEXED "blit" len lengths  =
      let src = create ~len 0.0 in
      let dst = create ~len 0.0 in
      (fun () -> Float.blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len)

    BENCH_INDEXED "blito" len lengths  =
      let src = create ~len 0.0 in
      let dst = create ~len 0.0 in
      (fun () -> Float.blito ~src ~src_pos:0 ~dst ~dst_pos:0 ~src_len:len ())
  end
end

open! Core_kernel
open! Pairing_heap

let%bench_fun ("pop_insert_with_existing_heap"[@indexed
                 initial_size
                 = [ 1; 100; 10_000; 1_000_000 ]])
  =
  let a = Array.init initial_size ~f:(fun _ -> Random.int 100_000) in
  let h1 = of_array ~cmp:Int.compare a in
  fun () ->
    let e = pop_exn h1 in
    add h1 e
;;

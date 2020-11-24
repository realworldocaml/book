open! Core_kernel
open! Fheap

let%bench_fun ("pop_add_with_existing_heap"[@indexed
                 initial_size = [ 1; 10; 100; 1000; 10_000 ]])
  =
  let a = Array.init initial_size ~f:(fun _ -> Random.int 100_000) in
  let h1 = of_array ~cmp:Int.compare a in
  fun () ->
    let e, h = pop_exn h1 in
    ignore (add h e : int t)
;;

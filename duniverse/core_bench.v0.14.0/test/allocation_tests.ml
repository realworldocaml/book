open Core
open Core_bench


let t1 = Bench.Test.create_indexed
  ~name:"alloc_list"
  ~args:[0; 10; 100; 400; 700; 1000; 5_000; 10_000; 50_000; 100_000]
  (fun words -> Staged.stage
    (fun () ->
      let rec loop n acc =
        if n = 0
        then acc
        else loop (n-1) (n::acc)
      in ignore (loop ((words / 3) + 1) [])))

let tests = [ t1 ]

let command = Bench.make_command tests


open Core
open Core_bench

let t1 = Bench.Test.create_indexed
  ~name:"ArrayCreateInt"
  ~args:[100;200;300;400;1000;10000]
  (fun len -> Staged.stage
    (fun () ->
      for _ = 0 to 2000 do
        ignore (Array.create ~len 0)
      done))

let tests = [ t1 ]

let command = Bench.make_command tests


open Core
open Core_bench


let t1 = Bench.Test.create ~name:"quick_stat"
  (fun () -> ignore(Gc.quick_stat ()))

let t2 = Bench.Test.create ~name:"counters"
  (fun () -> ignore(Gc.counters ()))

let t3 = Bench.Test.create ~name:"allocated_bytes"
  (fun () -> ignore(Gc.allocated_bytes ()))


let tests = [ t1; t2; t3 ]

let command = Bench.make_command tests

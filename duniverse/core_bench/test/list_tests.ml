open Core
open Core_bench

let t1 =
  Bench.Test.create_parameterised
    ~name:"ListAccess"
    ~args:[ "near_start", List.range 0 10; "near_end", List.range 9990 10_000 ]
    (fun indices ->
       let list = List.range 0 10_000 in
       Staged.stage (fun () ->
         for _ = 0 to 2000 do
           ignore (List.map indices ~f:(List.nth_exn list))
         done))
;;

let tests = [ t1 ]
let command = Bench.make_command tests

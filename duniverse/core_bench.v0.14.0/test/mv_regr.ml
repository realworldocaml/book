open Core
open Core_bench

let get_float () =
  if Random.bool ()
  then 10.0
  else 10.0

let get_int64 () =
  if Random.bool ()
  then Int64.of_int 10
  else 10L

module SimpleBenchmarks = struct

  let t1 =
    let arr1 = Array.create ~len:256 None in
    Bench.Test.create ~name:"SettingNone" (fun () ->
    for i = 0 to Array.length arr1 - 1 do
      arr1.(i) <- None;
    done)

  let arr2 = Array.create ~len:1000 0

  let t2 = Bench.Test.create ~name:"SettingInt" (fun () ->
    for i = 0 to Array.length arr2 - 1 do
      arr2.(i) <- 10;
    done)

  let t3 = Bench.Test.create ~name:"Id" (fun () -> ())

  let get_bool ()  =
    let a = Random.bool ()
    in if a then true else true

  let t4 = Bench.Test.create_indexed
    ~name:"ArrayCreateIntSize300"
    ~args:[100;200;300;400;1000;10000]
    (fun index ->
      let _dummy = Array.create ~len:index 0 in
      Staged.stage
      (fun () ->
        if (get_bool ()) then
          ignore (Array.create ~len:300 0)
        else ignore(_dummy)
      )
    )

  let t7 = Bench.Test.create
    ~name:"ArrayCreateInt1"
    (fun () -> ignore(Array.create ~len:200 0) )


  let all = [ t1; t2; t3; t4; t7 ]


end

let () =
  let testCases = List.concat( [
    SimpleBenchmarks.all;
  ] ) in

  Command.run (Bench.make_command testCases)


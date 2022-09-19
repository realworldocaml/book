open Core
open Core_bench

module Mutable = struct
  type t =
    { mutable iters : int
    ; mutable count : float
    }

  let rec test t =
    if t.iters = 0
    then ()
    else (
      t.iters <- t.iters - 1;
      t.count <- t.count +. 1.0;
      test t)
end

module Immutable = struct
  type t =
    { iters : int
    ; count : float
    }

  let rec test t =
    if t.iters = 0
    then ()
    else test { iters = t.iters - 1; count = t.count +. 1.0 }
end

let () =
  let iters = 1_000_000 in
  let count = 0.0 in
  let tests =
    [ Bench.Test.create ~name:"mutable" (fun () ->
          Mutable.test { iters; count })
    ; Bench.Test.create ~name:"immutable" (fun () ->
          Immutable.test { iters; count })
    ]
  in
  Bench.make_command tests |> Command_unix.run

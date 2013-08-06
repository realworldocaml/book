open Core.Std
open Core_bench.Std

type t1 = {
  x1: int;
}

type t2 = {
  mutable x2: int;
}

let functional_mutation () =
  let rec loop n acc =
    match n with
    |0 -> ()
    |n ->
      loop (n-1) { acc with x1=n }
  in loop 100 { x1=0 }

let imperative_mutation () =
  let rec loop n acc =
    match n with
    |0 -> ()
    |n -> 
      acc.x2 <- n;
      loop (n-1) acc
  in loop 100 { x2=0 }

let t1 =
  Bench.Test.create 
    ~name:"Functional Mutation"
    functional_mutation

let t2 =
  Bench.Test.create
    ~name:"Imperative Mutation"
    imperative_mutation

let () =
  Command.run (Bench.make_command [t1; t2])

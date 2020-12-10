open Core
open Core_bench

let get0 () =
  if Random.bool ()
  then 0
  else 0

let get1 () =
  if Random.bool ()
  then 1
  else 1


let f1_0 = Bench.Test.create ~name:"tuple1_0"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let (a, b) =
       match (x, y) with
       | 0, 0 -> 1, 1
       | x, y -> x+1, y+1
     in
     ignore (a + b)))

let f1_1 = Bench.Test.create ~name:"tuple1_1"
  (let x = get1 () in
   let y = get1 () in
   (fun () ->
     let (a, b) =
       match (x, y) with
       | 0, 0 -> 1, 1
       | x, y -> x+1, y+1
     in
     ignore (a + b)))

let f2_0 = Bench.Test.create ~name:"tuple2_0"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let a =
       match (x, y) with
       | 0, 0 -> 1 + 1
       | x, y -> x+1 + y+1
     in
     ignore (a)))

let f2_1 = Bench.Test.create ~name:"tuple2_1"
  (let x = get1 () in
   let y = get1 () in
   (fun () ->
     let a =
       match (x, y) with
       | 0, 0 -> 1 + 1
       | x, y -> x+1 + y+1
     in
     ignore (a)))

let f3 = Bench.Test.create ~name:"tuple3"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let (a, b) =
       match (x, y) with
       | a, b -> a, b
     in
     ignore (a + b)))

let f4 = Bench.Test.create ~name:"tuple4"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let (a, b) = (x, y) in
     ignore (a + b)))

let f5 = Bench.Test.create ~name:"tuple5"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let c = (x, y) in
     let (a, b) = c in
     ignore (a + b)))

let f6_0 = Bench.Test.create ~name:"tuple6_0"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let a =
       match (x, y) with
       | 1, 1 -> 10
       | _x -> 20
     in
     ignore a))

let f6_1 = Bench.Test.create ~name:"tuple6_1"
  (let x = get1 () in
   let y = get1 () in
   (fun () ->
     let a =
       match (x, y) with
       | 1, 1 -> 10
       | _x -> 20
     in
     ignore a))

let f7_0 = Bench.Test.create ~name:"tuple7_0"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let a =
       match (x, y) with
       | 1, 1 -> 10
       | x -> ignore x; 20
     in
     ignore a))

let f7_1 = Bench.Test.create ~name:"tuple7_1"
  (let x = get1 () in
   let y = get1 () in
   (fun () ->
     let a =
       match (x, y) with
       | 1, 1 -> 10
       | x -> ignore x; 20
     in
     ignore a))

let f8_0 = Bench.Test.create ~name:"tuple8_0"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let a =
       match (x, y) with
       | 1, 1 -> 10
       | a, b -> ignore a; ignore b; 20
     in
     ignore a))

let f8_1 = Bench.Test.create ~name:"tuple8_1"
  (let x = get1 () in
   let y = get1 () in
   (fun () ->
     let a =
       match (x, y) with
       | 1, 1 -> 10
       | a, b -> ignore a; ignore b; 20
     in
     ignore a))

let f9_0 = Bench.Test.create ~name:"tuple9_0"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let a =
       match (x, y) with
       | (1, 1) as p -> ignore p; 10
       | x, y -> x + y
     in
     ignore a))

let f9_1 = Bench.Test.create ~name:"tuple9_1"
  (let x = get1 () in
   let y = get1 () in
   (fun () ->
     let a =
       match (x, y) with
       | (1, 1) as p -> ignore p; 10
       | x, y -> x + y
     in
     ignore a))

let f10_0 = Bench.Test.create ~name:"tuple10_0"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let a =
       match (x, y) with
       | (1, y) as p -> ignore p; 1 + y
       | (x, y) -> x + y
     in
     ignore a))

let f10_1 = Bench.Test.create ~name:"tuple10_1"
  (let x = get1 () in
   let y = get1 () in
   (fun () ->
     let a =
       match (x, y) with
       | (1, y) as p -> ignore p; 1 + y
       | (x, y) -> x + y
     in
     ignore a))

let f11 = Bench.Test.create ~name:"tuple11"
  (let x = get0 () in
   let y = get0 () in
   (fun () ->
     let a =
       match (x, y) with
       | (x, y) as p -> ignore p; x + y
     in
     ignore a))

let tests = [ f1_0; f1_1;
              f2_0; f2_1;
              f3; f4; f5;
              f6_0; f6_1;
              f7_0; f7_1;
              f8_0; f8_1;
              f9_0; f9_1;
              f10_0; f10_1;
              f11 ]

let command = Bench.make_command tests


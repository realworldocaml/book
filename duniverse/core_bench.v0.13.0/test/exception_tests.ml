open Core
open Core_bench

exception Noarg
exception Arg1 of int

let get () =
  if Random.bool ()
  then 10
  else 10

let trywith = Bench.Test.create ~name:"trywith"
  (let x = get () in
   let y = get () in
   (fun () -> ignore(try x with _ -> y)))

let trywithraise0 = Bench.Test.create ~name:"trywith-raise0"
  (let x = get () in
   let y = get () in
   (fun () -> ignore(try raise Noarg with _ -> x+y)))

let trywithraise0match = Bench.Test.create ~name:"trywith-raise0-match"
  (let x = get () in
   let y = get () in
   (fun () -> ignore(try raise Noarg with Noarg -> x+y)))

let trywithraise1 = Bench.Test.create ~name:"trywith-raise1"
  (let x = get () in
   let y = get () in
   (fun () -> ignore(try raise (Arg1 x) with _ -> y)))

let trywithraise1match = Bench.Test.create ~name:"trywith-raise1-match"
  (let x = get () in
   let y = get () in
   (fun () -> ignore(try raise (Arg1 x) with (Arg1 x) -> x+y)))

let trywith1 = Bench.Test.create ~name:"trywith1"
  (let x = get () in
   let y = get () in
   (fun () -> ignore(try x with (Arg1 x) -> x+y)))

let recur d f () =
  let rec loop n =
    if n = 0
    then f ()
    else loop (n - 1) + 1
  in ignore (loop d)

let depths = [0; 10; 100; 1000; 10000]

let recur_trywith = Bench.Test.create_indexed
  ~name:"recur_trywith"
  ~args:depths
  (let x = get () in
   let y = get () in
   (fun depth ->
     Staged.stage (recur depth (fun () -> try x with _ -> y))))

let recur_trywithraise0 = Bench.Test.create_indexed
  ~name:"recur_trywith-raise0"
  ~args:depths
  (let x = get () in
   let y = get () in
   (fun depth ->
     Staged.stage (recur depth (fun () -> try raise Noarg with _ -> x+y))))

let recur_trywithraise0match = Bench.Test.create_indexed
  ~name:"recur_trywith-raise0-match"
  ~args:depths
  (let x = get () in
   let y = get () in
   (fun depth ->
     Staged.stage (recur depth (fun () -> try raise Noarg with Noarg -> x+y))))

let recur_trywithraise1 = Bench.Test.create_indexed
  ~name:"recur_trywith-raise1"
  ~args:depths
  (let x = get () in
   let y = get () in
   (fun depth ->
     Staged.stage (recur depth (fun () -> try raise (Arg1 x) with _ -> y))))

let recur_trywithraise1match = Bench.Test.create_indexed
  ~name:"recur_trywith-raise1-match"
  ~args:depths
  (let x = get () in
   let y = get () in
   (fun depth ->
     Staged.stage (recur depth (fun () -> try raise (Arg1 x) with (Arg1 x) -> x+y))))

let recur_trywith1 = Bench.Test.create_indexed
  ~name:"recurs_trywith1"
  ~args:depths
  (let x = get () in
   let y = get () in
   (fun depth ->
     Staged.stage (recur depth (fun () -> try x with (Arg1 x) -> x+y))))

let tests =
  [ trywith; trywithraise0; trywithraise0match; trywithraise1;
    trywithraise1match; trywith1;
    recur_trywith; recur_trywithraise0; recur_trywithraise0match; recur_trywithraise1;
    recur_trywithraise1match; recur_trywith1;
  ]

let command = Bench.make_command tests

open Core
open Core_bench

let get_float () =
  if Random.bool ()
  then 10.0
  else 10.0

let get_int () = Random.int 200000

let get_int64 () =
  if Random.bool ()
  then Int64.of_int 10
  else 10L

let scale t mul = Caml.int_of_float ((Caml.float_of_int t) *. mul)

let t1 = Bench.Test.create ~name:"Id"
  (fun () -> ())

let t2 =
  let n = get_int () in
  let fl = get_float () in
  Bench.Test.create ~name:"integer scaling"
  (fun () ->
    ignore (scale n fl))

let t3 = Bench.Test.create ~name:"Int64.bits_of_float"
  (let fl = get_float () in
   (fun () -> ignore (Int64.bits_of_float fl)))

let t4 = Bench.Test.create ~name:"Int64.float_of_bits"
  (let fl = get_int64 () in
   (fun () -> ignore (Int64.float_of_bits fl)))

let t5 =
  let f1 = Random.float 1.0 in
  let f2 = Random.float 1.0 in
  Bench.Test.create ~name:"Float.*"
    (fun () -> ignore (f1 *. f2))

let t6 =
  let f1 = Random.int 5000 in
  let f2 = Random.int 5000 in
  Bench.Test.create ~name:"Int.*"
    (fun () -> ignore (f1 * f2))

let tests = [ t1; t2; t3; t4; t5; t6 ]

let command = Bench.make_command tests



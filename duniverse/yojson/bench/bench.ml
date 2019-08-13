open Core
open Core_bench

let data =
  In_channel.read_all "bench.json"

let yojson_data = Yojson.Safe.from_string data

(* chosen by fair dice roll, guaranteed to be large *)
let large = 10_000

let large_int_assoc = 
  let ints = List.init large (fun n ->
   (string_of_int n, `Int n))
  in
  `Assoc ints

let large_int_list = 
  let ints = List.init large (fun n -> `Int n) in
  `List ints

let large_string_list =
  let strings = List.init large (fun n ->
    `String (string_of_int n))
  in
  `List strings

let main () =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"JSON reading" (fun () ->
      ignore (Yojson.Safe.from_string data));
    Bench.Test.create ~name:"JSON writing" (fun () ->
      ignore (Yojson.Safe.to_string yojson_data));
    Bench.Test.create ~name:"JSON writing assoc" (fun () ->
      ignore (Yojson.Safe.to_string large_int_assoc));
    Bench.Test.create ~name:"JSON writing int list" (fun () ->
      ignore (Yojson.Safe.to_string large_int_list));
    Bench.Test.create ~name:"JSON writing string list" (fun () ->
      ignore (Yojson.Safe.to_string large_string_list));
  ])

let () =
  main ()

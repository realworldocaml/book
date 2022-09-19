open Core
open Core_bench

let data =
  In_channel.read_all "bench.json"

let yojson_data = Yojson.Safe.from_string data

(* chosen by fair dice roll, guaranteed to be large *)
let large = 10_000

let large_int_assoc = 
  let ints = List.init large ~f:(fun n ->
   (string_of_int n, `Int n))
  in
  `Assoc ints

let large_int_list = 
  let ints = List.init large ~f:(fun n -> `Int n) in
  `List ints

let large_string_list =
  let strings = List.init large ~f:(fun n ->
    `String (string_of_int n))
  in
  `List strings

let streamable_string =
  let buf = Buffer.create (large * 100) in
  for i = 1 to large do
    Printf.bprintf buf "%d\n" i
  done;
  Buffer.contents buf

let generic =
  Bench.make_command [
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
    Bench.Test.create ~name:"JSON writing int list to channel" (fun () ->
      Out_channel.with_file "/dev/null" ~f:(fun oc ->
      ignore (Yojson.Safe.to_channel oc large_int_list)));
    Bench.Test.create ~name:"JSON writing string list to channel" (fun () ->
      Out_channel.with_file "/dev/null" ~f:(fun oc ->
      ignore (Yojson.Safe.to_channel oc large_string_list)));
    Bench.Test.create ~name:"JSON writing assoc to channel" (fun () ->
      Out_channel.with_file "/dev/null" ~f:(fun oc ->
      ignore (Yojson.Safe.to_channel oc large_int_assoc)));
    begin
      let buf = Buffer.create 1000 in
      Bench.Test.create ~name:"JSON seq roundtrip" (fun () ->
        let stream = Yojson.Safe.seq_from_string ~buf streamable_string in
        ignore (Yojson.Safe.seq_to_string ~buf stream)
      )
    end;
  ]

let buffer =
  let buf = Buffer.create 4096 in
  let data = large_int_assoc in
  Bench.make_command [
    Bench.Test.create ~name:"JSON writing with internal buffer" (fun () ->
      Out_channel.with_file "/dev/null" ~f:(fun oc ->
        ignore (Yojson.Safe.to_channel oc data)));
    Bench.Test.create ~name:"JSON writing with provided buffer" (fun () ->
      Out_channel.with_file "/dev/null" ~f:(fun oc ->
        ignore (Yojson.Safe.to_channel ~buf oc data)));
  ]

let main () =
  Command.group ~summary:"Benchmark" [
    ("generic", generic);
    ("buffer", buffer)
  ]
  |> Command_unix.run

let () =
  main ()

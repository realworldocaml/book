open Core.Std
open Core_bench.Std

let map_iter ~num_keys ~iterations =
  let rec loop i map =
    if i <= 0 then ()
    else loop (i - 1)
           (Map.change map (i mod num_keys) (fun current ->
              Some (1 + Option.value ~default:0 current)))
  in
  loop iterations Int.Map.empty

let table_iter ~num_keys ~iterations =
  let table = Int.Table.create ~size:num_keys () in
  let rec loop i =
    if i <= 0 then ()
    else (
      Hashtbl.change table (i mod num_keys) (fun current ->
        Some (1 + Option.value ~default:0 current));
      loop (i - 1)
    )
  in
  loop iterations

let tests ~num_keys ~iterations =
  let name container =
    sprintf "%s (#keys: %d, #iter: %d)" container num_keys iterations
  in
  [ Bench.Test.create ~name:(name "map")
      (fun () -> map_iter ~num_keys ~iterations)
  ; Bench.Test.create ~name:(name "table")
      (fun () -> table_iter ~num_keys ~iterations)
  ]

let () =
  Command.run (Bench.make_command (tests ~num_keys:1000 ~iterations:100_000))


open Core.Std
open Core_bench.Std

let create_maps ~num_keys ~iterations =
  let rec loop i map =
    if i <= 0 then []
    else
      let new_map =
        Map.change map (i mod num_keys) (fun current ->
          Some (1 + Option.value ~default:0 current))
      in
      new_map :: loop (i - 1) new_map
  in
  loop iterations Int.Map.empty

let create_tables ~num_keys ~iterations =
  let table = Int.Table.create ~size:num_keys () in
  let rec loop i =
    if i <= 0 then []
    else (
      Hashtbl.change table (i mod num_keys) (fun current ->
        Some (1 + Option.value ~default:0 current));
      let new_table = Hashtbl.copy table in
      new_table :: loop (i - 1)
    )
  in
  loop iterations

let tests ~num_keys ~iterations =
  let test name f = Bench.Test.create f ~name in
  [ test "map"   (fun () -> ignore (create_maps   ~num_keys ~iterations))
  ; test "table" (fun () -> ignore (create_tables ~num_keys ~iterations))
  ]

let () =
  tests ~num_keys:50 ~iterations:1000
  |> Bench.make_command
  |> Command.run

(* Correctness tests *)


let test ~num_keys ~iterations =
  let maps = create_maps ~num_keys ~iterations in
  let tables = create_tables ~num_keys ~iterations in
  let maps_of_tables =
    List.map tables ~f:(fun tbl ->
      Hashtbl.to_alist tbl |> Int.Map.of_alist_exn)
  in
  assert (List.equal ~equal:(Int.Map.equal Int.equal) maps maps_of_tables)

let () =
  test ~num_keys:50 ~iterations:10_000

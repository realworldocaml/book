open Core.Std
open Core_bench.Std

let plus_one_match x =
  match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | _ -> x + 1

let plus_one_if x =
  if      x = 0 then 1
  else if x = 1 then 2
  else if x = 2 then 3
  else x + 1

let () =
  (*let numbers = List.range 0 100 in
  let test name f =
    Bench.Test.create ~name (fun () ->
      List.iter ~f:(fun x -> ignore (f x)) numbers)
  in*)
  let test name f =
    Bench.Test.create ~name (fun () -> ignore (f 10))
  in
  [ test "plus_one_if"    plus_one_if
  ; test "plus_one_match" plus_one_match
  ]
  |> Bench.make_command
  |> Command.run

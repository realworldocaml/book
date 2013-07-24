open Core.Std
open Core_bench.Std

let simple_computation () =
  List.range 0 10
  |> List.fold ~init:0 ~f:(fun sum x -> sum + x * x)
  |> ignore

let simple_with_handler () =
  try simple_computation () with Exit -> ()

let end_with_exn () =
  try
    simple_computation ();
    raise Exit
  with Exit -> ()

let () =
  [ Bench.Test.create ~name:"simple computation"
      (fun () -> simple_computation ());
    Bench.Test.create ~name:"simple computation w/handler"
      (fun () -> simple_with_handler ());
    Bench.Test.create ~name:"end with exn"
      (fun () -> end_with_exn ());
  ]
  |> Bench.make_command
  |> Command.run

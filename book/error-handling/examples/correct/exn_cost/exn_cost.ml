open Core
open Core_bench

exception Exit

let x = 0

type how_to_end = Ordinary | Raise | Raise_no_backtrace

let computation how_to_end =
  let x = 10 in
  let y = 40 in
  let _z = x + (y * y) in
  match how_to_end with
  | Ordinary -> ()
  | Raise -> raise Exit
  | Raise_no_backtrace -> raise_notrace Exit

let computation_with_handler how = try computation how with Exit -> ()

let () =
  [
    Bench.Test.create ~name:"simple computation" (fun () ->
        computation Ordinary);
    Bench.Test.create ~name:"computation w/handler" (fun () ->
        computation_with_handler Ordinary);
    Bench.Test.create ~name:"end with exn" (fun () ->
        computation_with_handler Raise);
    Bench.Test.create ~name:"end with exn notrace" (fun () ->
        computation_with_handler Raise_no_backtrace);
  ]
  |> Bench.make_command |> Command.run

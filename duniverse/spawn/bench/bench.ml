open Core

let%bench "exec true with Spawn.spawn" =
  Spawn.spawn () ~prog:"/bin/true" ~argv:["true"]
  |> Pid.of_int
  |> Unix.waitpid

let%bench "exec true with Caml.Unix.create_process" =
  Caml.Unix.create_process "/bin/true" [|"true"|] Unix.stdin Unix.stdout Unix.stderr
  |> Pid.of_int
  |> Unix.waitpid


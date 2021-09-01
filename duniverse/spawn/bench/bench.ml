let%bench "exec true with Spawn.spawn" =
  Spawn.spawn () ~prog:"/bin/true" ~argv:[ "true" ] |> Unix.waitpid []

let%bench "exec true with Caml.Unix.create_process" =
  Unix.create_process "/bin/true" [| "true" |] Unix.stdin Unix.stdout
    Unix.stderr
  |> Unix.waitpid []

(** Test program for the bisect's signal handler.

    It forks: the parent process loops indefinitively,
    until the child process kills the parent with
    sigterm.

    If bisect has installed a signal handler we should see
    two coverage files: one from the parents sig handler and one
    from the childs at_exit hook.
*)
let () =
  let parent = Unix.getpid () in
  match Unix.fork () with
  | 0 -> Unix.kill parent Sys.sigterm
  | _ -> while true do Unix.sleep 5 done

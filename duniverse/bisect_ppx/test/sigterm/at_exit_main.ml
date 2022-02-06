let () =
  match Unix.fork () with
  | 0 ->
    At_exit_hook.is_child := true
  | child ->
    Unix.sleep 1;
    Unix.kill child Sys.sigterm;
    ignore @@ Unix.wait ()

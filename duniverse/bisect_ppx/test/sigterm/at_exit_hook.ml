[@@@coverage exclude_file]

let is_child = ref false

let () =
  at_exit (fun () ->
    if !is_child then begin
      Unix.sleep 5;
      print_endline "I should've been woken up by now."
    end)

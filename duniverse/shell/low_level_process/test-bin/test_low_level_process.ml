open Core
open OUnit

exception Got_the_first_line

let test =
  "process" >:::
  [ "early-exit" >::
    (fun () ->
       match
         Low_level_process.run
           ~prog:"bash"
           ~args:["-c"; "trap '' TERM; echo hello; sleep 46"]
           ~stdoutf:(fun s len ->
             if String.mem (Bytes.To_string.subo ~len s) '\n'
             then raise Got_the_first_line)
           ()
       with
       | exception Got_the_first_line ->
         ()
       | _ -> failwith "should have Got_the_first_line"
    )
  ]

let () = ignore (run_test_tt_main test)

open Core.Std
open Async.Std

(* Fail a thread with an exception *)
let fail ~mode () =
  printf "[%s] failure\n" mode;
  raise (Failure mode)

(* Return successfully from a thread *)
let succeed ~mode () =
  printf "[%s] success\n" mode;
  return ()

(* Run a function that spawns some threads that may raise exceptions *)
let f ~mode () =
  after (Time.Span.of_sec 0.1)
  >>= fun () ->
  match mode with
  |"ok" -> 
     printf "[%s] 1 thread immediate success\n" mode;
     succeed ~mode ()
  |"fail" ->
     printf "[%s] 1 thread immediate fail\n" mode;
     fail ~mode ()
  |"ok_ok" ->
     printf "[%s] 2 threads both succeed\n" mode;
     don't_wait_for (after (Time.Span.of_sec 0.2) >>= succeed ~mode);
     after (Time.Span.of_sec 0.1) >>= succeed ~mode
  |"fail_fail" ->
     printf "[%s] 2 threads both fail\n" mode;
     don't_wait_for (after (Time.Span.of_sec 0.2) >>= fail ~mode);
     after (Time.Span.of_sec 0.1) >>= fail ~mode
  |"ok_fail" ->
     printf "[%s] 2 threads: background fails first\n" mode;
     don't_wait_for (after (Time.Span.of_sec 0.1) >>= fail ~mode);
     after (Time.Span.of_sec 0.2) >>= succeed ~mode
  |"fail_ok" ->
     printf "[%s] 2 threads: main fails\n" mode;
     don't_wait_for (after (Time.Span.of_sec 0.2) >>= succeed ~mode);
     after (Time.Span.of_sec 0.1) >>= fail ~mode
  |_ -> assert false

(* Run the fake threads under a Monitor and check that the right
 * number of exceptions are received by the handler *)
let handle_and_expect ~mode ~num_errs ~f =
  let errs = ref 0 in
  let handle_exn exn = printf "handle_exn\n"; incr errs in
  Monitor.handle_errors (f ~mode) handle_exn
  >>= fun () ->
  assert(num_errs = !errs);
  after (Time.Span.of_sec 0.2)
  >>= fun () ->
  return (printf "%s: ok\n\n" mode)
  
let _ =
  printf "start\n";
  let mon = Monitor.current () in
  printf "current monitor: %s\n" (Info.to_string_hum (Monitor.name mon));
  handle_and_expect ~mode:"ok" ~num_errs:0 ~f
  >>= fun () ->
  handle_and_expect ~mode:"ok_ok" ~num_errs:0 ~f
  >>= fun () ->
  handle_and_expect ~mode:"ok_fail" ~num_errs:1 ~f
  >>= fun () ->
  handle_and_expect ~mode:"fail_ok" ~num_errs:1 ~f
  >>= fun () ->
  handle_and_expect ~mode:"fail" ~num_errs:1 ~f
  >>= fun () ->
  printf "end\n%!";
  return ()

let _ =
  Scheduler.go ()

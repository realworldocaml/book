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
     printf "[%s] 2 threads: background fails\n" mode;
     don't_wait_for (after (Time.Span.of_sec 0.2) >>= fail ~mode);
     after (Time.Span.of_sec 0.1) >>= succeed ~mode
  |"fail_ok" ->
     printf "[%s] 2 threads: main fails\n" mode;
     don't_wait_for (after (Time.Span.of_sec 0.2) >>= succeed ~mode);
     after (Time.Span.of_sec 0.1) >>= fail ~mode
  |_ -> assert false

(* Run the fake threads under a Monitor and check that the right
 * number of exceptions are received by the handler *)
let handle_and_expect ?(rest=`Ignore) ~mode ~expect ~f () =
  let name = "nested_"^mode in
  Monitor.try_with ~name ~run:`Schedule ~rest (f ~mode)
  >>= fun res ->
  (match expect,res with
  |`Ok, Result.Ok _
  |`Fail, Error _ -> ()
  |_ -> assert false);
  after (Time.Span.of_sec 0.2)
  >>= fun () ->
  return (printf "%s: ok\n\n" mode)
  
let _ =
  printf "start\n";
  let mon = Monitor.current () in
  printf "current monitor: %s\n" (Info.to_string_hum (Monitor.name mon));
  Monitor.try_with ~run:`Schedule ~rest:`Ignore (fun () ->
    handle_and_expect ~mode:"ok" ~expect:`Ok ~f ()
    >>= handle_and_expect ~mode:"fail" ~expect:`Fail ~f 
    >>= handle_and_expect ~mode:"ok_ok" ~expect:`Ok ~f
    >>= handle_and_expect ~mode:"ok_fail" ~expect:`Ok ~f
    >>= handle_and_expect ~mode:"fail_fail" ~expect:`Fail ~f
    >>= handle_and_expect ~mode:"fail_ok" ~expect:`Fail ~f
    >>= handle_and_expect ~rest:`Raise ~mode:"fail_ok" ~expect:`Fail ~f
    >>= handle_and_expect ~rest:`Raise ~mode:"ok_fail" ~expect:`Ok ~f
    >>= fun () -> return (printf "nested end\n%!")
  ) >>= fun res ->
  printf "testing outer monitor\n";
  Result.ok_exn res;
  return (printf "outer end\n%!")

let _ = Scheduler.go ()

open! Core
open! Async

let%expect_test "Flock" =
  Expect_test_helpers_async.with_temp_dir (fun tempdir ->
    let lock_path = tempdir ^/ "lock-file" in
    let second_thread_started = Ivar.create () in
    let%bind flock =
      match%map Lock_file_async.Flock.lock_exn ~lock_path with
      | `Somebody_else_took_it -> assert false
      | `We_took_it flock ->
        print_endline "original thread took lock";
        flock
    in
    let%bind () =
      let%bind () = Ivar.read second_thread_started in
      print_endline "original thread releasing lock";
      Lock_file_async.Flock.unlock_exn flock
    and () =
      let waiting_thread = Lock_file_async.Flock.wait_for_lock_exn ~lock_path () in
      print_endline "waiting thread started";
      Ivar.fill second_thread_started ();
      let%bind flock = waiting_thread in
      print_endline "waiting thread took lock";
      let%map () = Lock_file_async.Flock.unlock_exn flock in
      print_endline "waiting thread released lock"
    in
    let%bind () =
      [%expect
        {|
      original thread took lock
      waiting thread started
      original thread releasing lock
      waiting thread took lock
      waiting thread released lock |}]
    in
    return ())
;;

let%expect_test "Symlink" =
  Expect_test_helpers_async.with_temp_dir (fun tempdir ->
    let lock_path = tempdir ^/ "lock-symlink" in
    let second_thread_started = Ivar.create () in
    let%bind flock =
      match%map Lock_file_async.Symlink.lock_exn ~lock_path ~metadata:"original-thread" with
      | `Somebody_else_took_it _ -> assert false
      | `We_took_it flock ->
        print_endline "original thread took lock";
        flock
    in
    let%bind () =
      let%bind () = Ivar.read second_thread_started in
      print_endline "original thread releasing lock";
      Lock_file_async.Symlink.unlock_exn flock
    and () =
      let%bind ()
        =
        match%map
          Lock_file_async.Symlink.lock_exn ~lock_path ~metadata:"waiting-thread"
        with
        | `Somebody_else_took_it metadata ->
          print_s [%sexp
            `waiting_thread_sees
              (`lock_taken_by (metadata : string Or_error.t))]
        | `We_took_it _ ->
          assert false
      in
      let waiting_thread =
        Lock_file_async.Symlink.wait_for_lock_exn ~lock_path ~metadata:"waiting-thread" ()
      in
      Ivar.fill second_thread_started ();
      let%bind flock = waiting_thread in
      print_endline "waiting thread took lock";
      let%map () = Lock_file_async.Symlink.unlock_exn flock in
      print_endline "waiting thread released lock"
    in
    let%bind () =
      [%expect
        {|
      original thread took lock
      (waiting_thread_sees (lock_taken_by (Ok original-thread)))
      original thread releasing lock
      waiting thread took lock
      waiting thread released lock |}]
    in
    return ())
;;

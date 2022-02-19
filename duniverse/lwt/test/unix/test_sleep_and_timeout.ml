(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test
open Lwt.Infix

(* None of the APIs make promises about how much larger the elapsed time will
 * be, but they all promise that it won't be less than the expected time. *)
let cmp_elapsed_time test_name start_time expected_time =
  let elapsed_time = Unix.gettimeofday () -. start_time in
  let diff = elapsed_time -. expected_time in
  let result = diff >= 0. && diff <= 0.2 in
  instrument result "Lwt_unix sleep and timeout: %s: %f %f %f %b"
    test_name elapsed_time expected_time diff (Lwt_sys.have `libev)

let suite = suite "Lwt_unix sleep and timeout" [
    test "sleep" begin fun () ->
      let start_time = Unix.gettimeofday () in
      let duration = 1.0 in
      Lwt_unix.sleep duration
      >>= fun () ->
      let check = cmp_elapsed_time "sleep" start_time duration in
      Lwt.return check
    end;

    test "timeout" begin fun () ->
      let start_time = Unix.gettimeofday () in
      let duration = 1.0 in
      Lwt.catch
        (fun () ->
           Lwt_unix.timeout duration
           >>= fun () -> Lwt.return_false
        )
        (function
          | Lwt_unix.Timeout ->
            let check = cmp_elapsed_time "timeout" start_time duration in
            Lwt.return check
          | exn -> Lwt.fail exn
        )
    end;

    test "with_timeout : no timeout" begin fun () ->
      let duration = 1.0 in
      Lwt_unix.with_timeout duration Lwt.pause
      >>= fun () -> Lwt.return_true
    end;

    test "with_timeout : timeout" begin fun () ->
      let start_time = Unix.gettimeofday () in
      let duration = 1.0 in
      let f () = Lwt_unix.sleep 2.0 in
      Lwt.catch
        (fun () ->
           Lwt_unix.with_timeout duration f
           >>= fun () ->
           Printf.eprintf "\nno timeout\n";
           Lwt.return false
        )
        (function
          | Lwt_unix.Timeout ->
            let check =
              cmp_elapsed_time "with_timeout : timeout" start_time duration in
            Lwt.return check
          | exn -> Lwt.fail exn
        )
    end;

    test "pause" begin fun () ->
      let bind_callback_ran = ref false in
      Lwt.async (fun () -> Lwt.return () >|= fun () -> bind_callback_ran := true);
      let bind_is_immediate = !bind_callback_ran in
      let pause_callback_ran = ref false in
      Lwt.async (fun () -> Lwt.pause () >|= fun () -> pause_callback_ran := true);
      let pause_is_immediate = !pause_callback_ran in
      Lwt.return (bind_is_immediate && not pause_is_immediate)
    end;

    test "auto_pause" begin fun () ->
      let f = Lwt_unix.auto_pause 1.0 in
      let run_auto_pause () =
        let callback_ran = ref false in
        Lwt.async (fun () -> f () >|= fun () -> callback_ran := true);
        !callback_ran;
      in
      let check1 = run_auto_pause () in
      let check2 = run_auto_pause () in
      Lwt_unix.sleep 1.0
      >|= fun () ->
      let check3 = run_auto_pause () in
      let check4 = run_auto_pause () in
      let check5 = run_auto_pause () in
      let check = check1 && check2 && not check3 && check4 && check5 in
      instrument check "Lwt_unix sleep and timeout: auto_pause: %b %b %b %b %b"
        check1 check2 check3 check4 check5
    end;
  ]

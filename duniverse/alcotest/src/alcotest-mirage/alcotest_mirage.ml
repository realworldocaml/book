module Make (C : Mirage_clock.MCLOCK) = struct
  module Platform (M : Alcotest_engine.Monad.S) = struct
    let time () = Duration.to_f @@ C.elapsed_ns ()
    let getcwd () = ""
    let stdout_isatty () = true
    let stdout_columns () = None
    let setup_std_outputs ?style_renderer:_ ?utf_8:_ () = ()

    (* Pre-4.07 doesn't support empty variant types. *)
    type file_descriptor = { empty : 'a. 'a }

    let log_trap_supported = false
    let prepare_log_trap ~root:_ = assert false
    let file_exists _ = assert false
    let open_write_only _ = assert false
    let close = function (fd : file_descriptor) -> fd.empty
    let with_redirect = function (fd : file_descriptor) -> fd.empty

    let home_directory () =
      Error (`Msg "Home directory not available for the MirageOS platform")
  end

  module Tester = Alcotest_engine.V1.Cli.Make (Platform) (Lwt)
  include Tester

  let test_case_sync n s f = test_case n s (fun x -> Lwt.return (f x))

  let run_test fn args =
    let async_ex, async_waker = Lwt.wait () in
    let handle_exn ex =
      Logs.debug (fun f -> f "Uncaught async exception: %a" Fmt.exn ex);
      if Lwt.state async_ex = Lwt.Sleep then Lwt.wakeup_exn async_waker ex
    in
    Lwt.async_exception_hook := handle_exn;
    Lwt_switch.with_switch (fun sw -> Lwt.pick [ fn sw args; async_ex ])

  let test_case n s f = test_case n s (run_test f)
end

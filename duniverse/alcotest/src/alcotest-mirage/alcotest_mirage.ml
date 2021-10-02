module Make (C : Mirage_clock.MCLOCK) = struct
  module Platform (M : Alcotest_engine.Monad.S) = struct
    let time () = Duration.to_f @@ C.elapsed_ns ()
    let getcwd () = ""
    let prepare ~base:_ ~dir:_ ~name:_ = ()
    let stdout_isatty () = true
    let stdout_columns () = None
    let with_redirect _ fn = fn ()
    let setup_std_outputs ?style_renderer:_ ?utf_8:_ () = ()

    let home_directory () =
      Error (`Msg "Home directory not available for the MirageOS platform")
  end

  module Tester = Alcotest_engine.Cli.Make (Platform) (Lwt)
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

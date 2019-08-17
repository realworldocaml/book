(* This code is in the public domain. *)

(* Example for installing multiple reporters. *)

let combine r1 r2 =
  let report = fun src level ~over k msgf ->
    let v = r1.Logs.report src level ~over:(fun () -> ()) k msgf in
    r2.Logs.report src level ~over (fun () -> v) msgf
  in
  { Logs.report }

let () =
  let r1 = Logs.format_reporter () in
  let r2 = Logs_fmt.reporter () in
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (combine r1 r2);
  Logs.err (fun m -> m "HEY HO!");
  ()

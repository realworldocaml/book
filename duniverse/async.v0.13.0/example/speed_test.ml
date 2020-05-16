open Core
open Async

let run_test ~fill_before_upon ~no_ivars ~spawn_factor =
  let spawn =
    let spawns = List.range 0 spawn_factor in
    fun i -> List.iter spawns ~f:(fun _ -> upon (Ivar.read i) Fn.ignore)
  in
  let finished = Ivar.create () in
  let rec loop n =
    if n > 0
    then (
      let i = Ivar.create () in
      if fill_before_upon
      then (
        Ivar.fill i ();
        spawn i)
      else (
        spawn i;
        Ivar.fill i ());
      loop (n - 1))
    else Ivar.fill finished ()
  in
  loop no_ivars;
  Ivar.read finished
;;

let () =
  let no_ivars = int_of_string (Sys.get_argv ()).(1) in
  let spawn_factor = int_of_string (Sys.get_argv ()).(2) in
  if spawn_factor >= 20 then failwith "spawn_factor must be less than 20";
  let fill_before_upon =
    match (Sys.get_argv ()).(3) with
    | "fill" -> true
    | "upon" -> false
    | _ -> failwith "must specify either 'fill' or 'upon'"
  in
  let start = Time.now () in
  upon (run_test ~fill_before_upon ~no_ivars ~spawn_factor) (fun () ->
    let stop = Time.now () in
    Core.Printf.printf
      "elapsed time: %s\n"
      (Time.Span.to_string (Time.diff stop start));
    Shutdown.shutdown 0);
  never_returns (Scheduler.go ())
;;

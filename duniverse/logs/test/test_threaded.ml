let loop s =
  for _ = 0 to 10 do
    Logs.info (fun f -> f "%s.%s" s s)
  done

let () =
  Logs_threaded.enable ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  let t1 = Thread.create loop "aaaa" in
  let t2 = Thread.create loop "bbbb" in
  loop "cccc";
  Thread.join t1;
  Thread.join t2

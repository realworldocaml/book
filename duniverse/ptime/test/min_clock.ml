(*
   Compile with:

   ocamlfind ocamlopt \
     -package ptime.clock.os -linkpkg -o min_clock.native min_clock.ml

   ocamlfind ocamlc \
     -package ptime.clock.os -linkpkg -o min_clock.byte min_clock.ml

   js_of_ocaml \
     $(ocamlfind query ptime.clock.os -predicates javascript -o-format -r) \
     min_clock.byte
*)

let pp_period ppf = function
| None -> Format.fprintf ppf "unknown"
| Some p -> Ptime.Span.pp ppf p

let pp_tz ppf = function
| None -> Format.fprintf ppf "unknown"
| Some tz -> Format.fprintf ppf "%ds" tz

let main () =
  let now = Ptime_clock.now () in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let period = Ptime_clock.period () in
  Format.printf "Clock period: %a@." pp_period period;
  Format.printf "   TZ offset: %a@." pp_tz tz_offset_s;
  Format.printf "   Now UTC  : %a@." Ptime.pp now;
  Format.printf "   Now local: %a@." Ptime.(pp_human ?tz_offset_s ()) now;
  ()

let () = if !Sys.interactive then () else main ()

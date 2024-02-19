(*
   Compile with:

   ocamlfind ocamlopt \
     -package mtime.clock.os -linkpkg -o min_clock.native min_clock.ml

   ocamlfind ocamlc \
     -package mtime.clock.os -linkpkg -o min_clock.byte min_clock.ml

    js_of_ocaml \
      $(ocamlfind query -format "%+(jsoo_runtime)" -r mtime.clock.os) \
      min_clock.byte
*)

let main () =
  Format.printf "Elapsed: %a@." Mtime.Span.pp (Mtime_clock.elapsed ());
  Format.printf "Timestamp: %a@." Mtime.pp (Mtime_clock.now ());
  Format.printf "Clock period: %s@."
    (match Mtime_clock.period () with
    | None -> "unknown" | Some s -> Format.asprintf "%a" Mtime.Span.pp s);
  ()

let () = if !Sys.interactive then () else main ()

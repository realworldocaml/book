(*
   Compile with:
    ocamlfind ocamlc \
       -package mtime,mtime.clock.os -linkpkg -o min_os.byte min_os.ml
    ocamlfind ocamlopt \
       -package mtime,mtime.clock.os -linkpkg -o min_os.native min_os.ml *)

let () =
  Format.printf "Elapsed: %a@." Mtime.Span.pp (Mtime_clock.elapsed ());
  Format.printf "Timestamp: %a@." Mtime.pp (Mtime_clock.now ());
  Format.printf "Clock period: %s@."
    begin match Mtime_clock.period () with
    | None -> "unknown" | Some s -> Format.asprintf "%a" Mtime.Span.pp s
    end;
  ()

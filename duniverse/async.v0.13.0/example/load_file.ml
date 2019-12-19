open Core
open Async

let lines file =
  Reader.with_file file ~f:(fun reader ->
    Deferred.create (fun i ->
      let rec loop ac =
        upon (Reader.read_line reader) (function
          | `Eof -> Ivar.fill i (Array.of_list (List.rev ac))
          | `Ok line -> loop (line :: ac))
      in
      loop []))
;;

(*     (fun reader ->
       Reader.contents reader >>| fun contents ->
       Array.of_list (String.split ~on:'\n' contents)) *)
(*
   let lines_stream = Reader.lines reader in
   Stream.to_list lines_stream >>| Array.of_list) *)

let main () =
  let file = (Sys.get_argv ()).(1) in
  (*  Gc.set
      { (Gc.get ()) with Gc.Control.
      minor_heap_size = 8_388_608;
      space_overhead = 150;
      max_overhead = 1_000_000;
      major_heap_increment = 1_048_576 }; *)
  upon (lines file) (fun _lines -> Shutdown.shutdown 0);
  never_returns (Scheduler.go ())
;;

let () = Exn.handle_uncaught ~exit:true main

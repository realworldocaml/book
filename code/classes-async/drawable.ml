open Core.Std
open Async.Std
open Async_graphics

type drawable = < draw: unit >

let shapes: drawable list ref = ref []

let repaint () =
  try 
    clear_graph ();
    List.iter ~f:(fun s -> s#draw) !shapes;
    synchronize ()
  with Graphic_failure _ -> ()

let () = 
  Clock.every (Time.Span.of_sec (1.0 /. 24.0)) repaint

let open_display () =
  close_graph ();
  open_graph "";
  auto_synchronize false

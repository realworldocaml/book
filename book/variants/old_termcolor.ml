#require "core,core.top";;

type basic_color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
type weight = Regular | Bold
type color =
    | Basic of basic_color * weight (* basic colors, regular and bold *)
    | RGB   of int * int * int      (* 6x6x6 color cube *)
    | Gray  of int                  (* 24 grayscale levels *)
let basic_color_to_int = function
    | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
    | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7

let () = Printexc.record_backtrace false

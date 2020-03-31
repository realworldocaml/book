open Unix
open Printf

type chrono =
  float ref

let fresh () =
  ref 0.

let chrono (chrono : float ref) (task : unit -> 'a) : 'a =
  let times1 = times() in
  let result = task() in
  let times2 = times() in
  chrono := !chrono +. times2.tms_utime -. times1.tms_utime;
  result

let display channel (chrono : float ref) =
  fprintf channel "%.02f\n" !chrono

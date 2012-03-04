open Core.Std

let rec recur () =
  1 + recur ()

let z = recur ()

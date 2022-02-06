let f () =
  ()

let g () =
  ()

let () =
  f ()

(* Reproduces a HTML display bug that existed in development between 2.6.3 and
   2.7.0, starting with 1b8d7ec5985aa12a85e797e3d53fc72713e80c35. *)
let a () =
  if true then
    true
  else
    false

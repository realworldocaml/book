open Core.Std

let a () = let _ = "a" in raise Not_found
let b () = let _ = "b" in a ()

let c () =
  let _ = "c" in
  protect ~f:b
    ~finally:(fun () -> ())

let d () = let _ = "d" in c ()
let () = d ()

let f () =
  lwt x = g () in
  Lwt.return x

let f x = match_lwt x with
  | A -> A
  | B -> B

let g x = try_lwt
    f x
  finally
    g x

let a f x =
  try_lwt f x
  with Failure _ -> ()
  finally ()

(* should'nt break normal try/with imbrication *)
let z f x =
  try
    try f x
    with Exit -> ()
  with _ -> ()

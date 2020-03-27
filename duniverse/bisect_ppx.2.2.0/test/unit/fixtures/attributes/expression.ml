let fn _ =
  ()

let () =
  if true then
    fn 1 [@coverage off]
  else
    fn 2;;

fn 3;;

fn 4 [@coverage off];;

(fn (if true then 5 else 6)) [@coverage off];;

(* Application expressions that place their marks visually on another
   expression. *)
let () =
  fn ()

let () =
  (fn [@coverage off]) ()

let () =
  fn (); ()

let () =
  fn (); (() [@coverage off])

let () =
  fn @@ ()

let () =
  (fn [@coverage off]) @@ ()

let () =
  () |> fn

let () =
  () |> (fn [@coverage off])

let fn' _ _ =
  ()

let () =
  () |> fn' ()

let () =
  () |> (fn' () [@coverage off])

let () =
  () |> (fn' [@coverage off]) ()

let () =
  () |> fn; ()

let () =
  () |> fn; (() [@coverage off])

let _ =
  true || false

let _ =
  true [@coverage off] || false

let _ =
  true || false [@coverage off]

let _ =
  true || true || true [@coverage off]

let _ =
  true || true [@coverage off] || true

class foo =
  object
    method bar =
      ()
  end

let () =
  let _ = new foo in
  ()

let () =
  let _ = new foo in
  (() [@coverage off])

let () =
  let o = new foo in
  o#bar; ()

let () =
  let o = new foo in
  o#bar; (() [@coverage off])

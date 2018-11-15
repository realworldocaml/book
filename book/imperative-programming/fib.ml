let time f =
  let open Core in
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
  printf "Time: %F ms\n" (Time.diff stop start |> Time.Span.to_ms);
  x

let memoize f =
  let table = Hashtbl.Poly.create () in
  (fun x ->
     match Hashtbl.find table x with
     | Some y -> y
     | None ->
       let y = f x in
       Hashtbl.add_exn table ~key:x ~data:y;
       y
  )

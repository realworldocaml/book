let time f =
  let open Core in
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
  printf "Time: %F ms\n" (Time.diff stop start |> Time.Span.to_ms);
  x

let memoize f =
  let memo_table = Hashtbl.Poly.create () in
  (fun x ->
     Hashtbl.find_or_add memo_table x ~default:(fun () -> f x))

let fib_norec fib i =
  if i <= 1 then i
  else fib (i - 1) + fib (i - 2)

let identity x = x

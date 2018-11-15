let memoize f =
  let memo_table = Hashtbl.Poly.create () in
  (fun x ->
     Hashtbl.find_or_add memo_table x ~default:(fun () ->
f x))

let memo_rec f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize (fun x -> f_norec !fref x) in
  fref := f;
  f x

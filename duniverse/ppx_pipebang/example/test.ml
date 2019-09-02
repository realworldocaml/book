let f x = x
let g x = x
let h x = x
let _ =
  (1 |> f) |> (g |> h)

type 'a t  = X | F of 'a
let _constr_on_left  f = X |> f
let _constr_on_right x = x |> F

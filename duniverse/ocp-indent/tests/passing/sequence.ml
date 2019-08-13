let f = fun x ->
  x

let f x =
  x

let f g = fun x -> g
    x

let f g x = g
    x

let l1 = (a ::
          b ::
          [])

let l1 = (
  a ::
  b ::
  [])

let l1 =
  a ::
  b ::
  []

let l1 = a ::
         b ::
         []

let l1 = [a;
          b;
         ]

let l1 = [
  a;
  b;
]

let l1 =
  [ a;
    b;
  ]

let l1 =
  [ a
  ; b
  ]

let l1 = [
  a
; b
]

let l1 =
  [     a;
        b
  ; c
  ]

let f1 = function
  | {k=A|B} -> true
  | {k=C} -> false

let overflow_small =
  4611686018427387904 (* max_int (63) + 1 *)
let overflow_big =
  46116860184273879030

let ppx_sequence =
  ();%ext
  ()

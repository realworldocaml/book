type t

external make : LDouble.t -> LDouble.t -> t = "ctypes_ldouble_complex_make"

external re : t -> LDouble.t = "ctypes_ldouble_complex_real"
external im : t -> LDouble.t = "ctypes_ldouble_complex_imag"

let of_complex x = make (LDouble.of_float x.Complex.re) (LDouble.of_float x.Complex.im)
let to_complex x = { Complex.re = LDouble.to_float (re x); im = LDouble.to_float (im x) }

let norm2 x = 
  let r, i = re x, im x in
  LDouble.(add (mul r r) (mul i i))

let norm x = 
  let open LDouble in
  let r = abs (re x) and i = abs (im x) in
  if r = zero then i
  else if i = zero then r
  else if r >= i then
    let q = div i r in mul r (sqrt (add one (mul q q)))
  else
    let q = div r i in mul i (sqrt (add one (mul q q)))

let polar n a = make LDouble.(mul (cos a) n) LDouble.(mul (sin a) n)

let zero = make LDouble.zero LDouble.zero
let one = make LDouble.one LDouble.zero
let i = make LDouble.zero LDouble.one

external neg : t -> t = "ctypes_ldouble_complex_neg"
external conj : t -> t = "ctypes_ldouble_complex_conjl"
external add : t -> t -> t = "ctypes_ldouble_complex_add"
external sub : t -> t -> t = "ctypes_ldouble_complex_sub"
external mul : t -> t -> t = "ctypes_ldouble_complex_mul"
external div : t -> t -> t = "ctypes_ldouble_complex_div"
let inv x = div one x
external sqrt : t -> t = "ctypes_ldouble_complex_csqrtl"
external arg : t -> LDouble.t = "ctypes_ldouble_complex_cargl"
external exp : t -> t = "ctypes_ldouble_complex_cexpl"
external log : t -> t = "ctypes_ldouble_complex_clogl"
external pow : t -> t -> t = "ctypes_ldouble_complex_cpowl"


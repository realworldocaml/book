open OUnit2

open Nocrypto
open Nocrypto.Uncommon

let (prf, strf) = Format.(fprintf, asprintf)
let xd = xd ()
let pp_map pp f ppf x = pp ppf (f x)
let pp_diff pp ppf (a, b) = prf ppf "@[<v>want: %a@,have: %a@]" pp a pp b

let rec blocks_of_cs n cs =
  let open Cstruct in
  if len cs <= n then [ cs ]
  else sub cs 0 n :: blocks_of_cs n (shift cs n)

let rec range a b =
  if a > b then [] else a :: range (succ a) b

let rec times ~n f a =
  if n > 0 then ( ignore (f a) ; times ~n:(pred n) f a )

let pp_opt pp ppf = Format.(function
  | Some x -> fprintf ppf "Some(%a)" pp x
  | None   -> fprintf ppf "None")

let eq_opt eq a b = match (a, b) with
  | (Some x, Some y) -> eq x y
  | _                -> false

let sample arr =
  let ix = Rng.Int.gen Array.(length arr) in arr.(ix)

let assert_cs_equal ?msg =
  assert_equal ~cmp:Cstruct.equal ?msg
    ~pp_diff:(pp_diff (Uncommon.xd ~ascii:true()))

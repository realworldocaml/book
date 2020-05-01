open OUnit2

let (prf, strf) = Format.(fprintf, asprintf)
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

let assert_cs_equal ?msg =
  assert_equal ~cmp:Cstruct.equal ?msg
    ~pp_diff:(pp_diff Cstruct.hexdump_pp)

let iter_list xs f = List.iter f xs

let cases_of f =
  List.map @@ fun params -> test_case (f params)

let any _ = true

let vx = Cstruct.of_hex

let f1_eq ?msg f (a, b) _ =
  assert_cs_equal ?msg (f (vx a)) (vx b)

let f1_opt_eq ?msg f (a, b) _ =
  let maybe = function None -> None | Some h -> Some (vx h) in
  let (a, b) = vx a, maybe b in
  let eq_opt eq a b = match (a, b) with
    | (Some x, Some y) -> eq x y
    | (None  , None  ) -> true
    | _                -> false
  in
  assert_equal b (f a) ?msg
    ~cmp:(eq_opt Cstruct.equal)
    ~pp_diff:(pp_diff (pp_opt Cstruct.hexdump_pp))

let f2_eq ?msg f (a, b, c) = f1_eq ?msg (f (vx a)) (b, c)

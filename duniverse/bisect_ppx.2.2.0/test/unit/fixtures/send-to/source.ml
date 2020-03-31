let inf = ref 0
let sup = ref 3

let args = [
  ("-inf", (Arg.Set_int inf), "inferior bound") ;
  ("-sup", (Arg.Set_int sup), "superior bound")
]

let kind = function
  | x when x > 9 || x < 0 -> print_endline "not a digit"
  | _ -> print_endline "digit"

let print x =
  print_int x;
  print_newline ()

let () =
  Arg.parse args ignore "report test";
  for i = !inf to !sup do
    kind i;
    print i
  done

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n/2) in
      b * b * (if n mod 2 = 0 then 1 else a)

let () =
  assert ((pow 2 2) = 4)

let is_even x =
  if x mod 2 = 0 then true else false

let rec is_odd x =
  match x with
  | 1
  | 3
  | 5
  | 7
  | 9 -> true
  | x -> if is_even x then false else is_odd (x mod 10)

let is_zero x =
  try
    let _ = 20 / x in
    false
  with Division_by_zero -> true

let () =
  (fun x ->
    match x with
    | 0, 0 -> 0
    | _ -> 1)
  (0, 0)
  |> ignore

let f _ = "a string to confound Buffer.add_substitute $("

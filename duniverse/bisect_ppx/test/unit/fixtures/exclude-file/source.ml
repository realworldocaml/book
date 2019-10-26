let f1 x y =
  if x = y then
    x + y
  else
    x - y

let g s =
  for i = 1 to 5 do
    print_endline s
  done

let f2 b x =
  if b then
    x * x
  else
    x

let f3 : type a. a -> string = fun _ -> "Hello"

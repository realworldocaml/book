open Core.Std


let rec twothree n =
  printf "%d\n" n;
  if n = 1 then ()
  else if n % 2 = 0
  then twothree (n/2)
  else twothree (n * 3 + 1)



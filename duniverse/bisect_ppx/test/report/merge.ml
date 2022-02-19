let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs

let rec product = function
  | [] -> 1
  | x :: xs -> x * product xs


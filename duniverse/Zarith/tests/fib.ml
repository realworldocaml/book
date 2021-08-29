let rec fib_int n =
  if n < 2 then 1 else fib_int(n-1) + fib_int(n-2)

let rec fib_natint n =
  if n < 2 then 1n else Nativeint.add (fib_natint(n-1)) (fib_natint(n-2))

let rec fib_z n =
  if n < 2 then Z.one else Z.add (fib_z(n-1)) (fib_z(n-2))

open Big_int

let rec fib_bi n =
  if n < 2 then unit_big_int else add_big_int (fib_bi(n-1)) (fib_bi(n-2))

open Num

let rec fib_num n =
  if n < 2 then Int 1 else add_num (fib_num(n-1)) (fib_num(n-2))

let _ =
  let n = int_of_string Sys.argv.(2) in
  match Sys.argv.(1) with
  | "int" ->
      Printf.printf "%d\n" (fib_int n)
  | "natint" ->
      Printf.printf "%nd\n" (fib_natint n)
  | "Z" -> 
      Printf.printf "%a\n" Z.output (fib_z n)
  | "bigint" -> 
      Printf.printf "%s\n" (string_of_big_int (fib_bi n))
  | "num" ->
      Printf.printf "%s\n" (string_of_num (fib_num n))

  | _ ->
      ()


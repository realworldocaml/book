let rec fact_z n =
  if n <= 0 then Z.one else Z.mul (Z.of_int n) (fact_z (n-1))

let rec repeat n f arg =
  if n <= 0 then f arg else begin ignore (f arg); repeat (n-1) f arg end

let _ =
  let n = int_of_string Sys.argv.(1) in
  let c = int_of_string Sys.argv.(2) in
  Printf.printf "%a\n" Z.output (repeat c fact_z n)

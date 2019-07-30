let rec tak_int (x, y, z) =
  if x > y
  then tak_int(tak_int (x-1, y, z), tak_int (y-1, z, x), tak_int (z-1, x, y))
  else z

let rec tak_natint (x, y, z) =
  if x > y
  then tak_natint(tak_natint (Nativeint.sub x 1n, y, z),
                  tak_natint (Nativeint.sub y 1n, z, x),
                  tak_natint (Nativeint.sub z 1n, x, y))
  else z

let rec tak_z (x, y, z) =
  if Z.compare x y > 0
  then tak_z(tak_z (Z.pred x, y, z),
             tak_z (Z.pred y, z, x),
             tak_z (Z.pred z, x, y))
  else z

open Big_int

let rec tak_bi (x, y, z) =
  if gt_big_int x y
  then tak_bi(tak_bi (pred_big_int x, y, z),
              tak_bi (pred_big_int y, z, x),
              tak_bi (pred_big_int z, x, y))
  else z

open Num

let rec tak_num (x, y, z) =
  if x >/ y
  then tak_num(tak_num (pred_num x, y, z),
               tak_num (pred_num y, z, x),
               tak_num (pred_num z, x, y))
  else z

let rec repeat n f arg =
  if n <= 0 then f arg else begin ignore (f arg); repeat (n-1) f arg end

let _ =
  let n = int_of_string Sys.argv.(2) in
  match Sys.argv.(1) with
  | "int" ->
      Printf.printf "%d\n" (repeat n tak_int (18,12,6))
  | "natint" ->
      Printf.printf "%nd\n" (repeat n tak_natint (18n,12n,6n))
  | "Z" -> 
      Printf.printf "%a\n" Z.output (repeat n tak_z (Z.of_int 18, Z.of_int 12, Z.of_int 6))
  | "num" -> 
      Printf.printf "%s\n" (string_of_num (repeat n tak_num (Int 18, Int 12, Int 6)))
  | "bigint" -> 
      Printf.printf "%s\n" (string_of_big_int (repeat n tak_bi (big_int_of_int 18, big_int_of_int 12, big_int_of_int 6)))
  | _ ->
      ()

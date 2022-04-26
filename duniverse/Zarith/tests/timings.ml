open Printf

(* Timing harness harness *)

let time fn arg =
  let start = Sys.time() in
  let rec time accu =
    let qty = fn arg in
    let duration = Sys.time() -. start in
    let qty = float qty in
    if duration >= 1.0
    then duration /. (accu +. qty)
    else time (accu +. qty)
  in time 0.0

let time_repeat rep fn arg =
  time (fun () -> for i = 1 to rep do ignore (fn arg) done; rep) ()

(* Basic arithmetic operations *)

let add (x, y) =
  for i = 1 to 50_000_000 do
    ignore (Sys.opaque_identity (Z.add x y))
  done;
  50_000_000

let sub (x, y) =
  for i = 1 to 50_000_000 do
    ignore (Sys.opaque_identity (Z.sub x y))
  done;
  50_000_000

let mul (x, y) =
  for i = 1 to 50_000_000 do
    ignore (Sys.opaque_identity (Z.mul x y))
  done;
  50_000_000

let div (x, y) =
  for i = 1 to 10_000_000 do
    ignore (Sys.opaque_identity (Z.div x y))
  done;
  1_000_000

let shl (x, y) =
  for i = 1 to 50_000_000 do
    ignore (Sys.opaque_identity (Z.shift_left x y))
  done;
  50_000_000

let big = Z.pow (Z.of_int 17) 150
let med = Z.pow (Z.of_int  3) 150

let _ =
  printf "%.2e    add (small, no overflow)\n%!"
         (time add (Z.of_int 1, Z.of_int 2));
  printf "%.2e    add (small, overflow)\n%!"
         (time add (Z.of_int max_int, Z.of_int 2));
  printf "%.2e    add (small, big)\n%!"
         (time add (Z.of_int 1, big));
  printf "%.2e    add (big, big)\n%!"
         (time add (big, big));
  printf "%.2e    sub (small, no overflow)\n%!"
         (time sub (Z.of_int 1, Z.of_int 2));
  printf "%.2e    sub (small, overflow)\n%!"
         (time sub (Z.of_int max_int, Z.of_int (-2)));
  printf "%.2e    sub (big, small)\n%!"
         (time sub (big, Z.of_int 1));
  printf "%.2e    sub (big, big)\n%!"
         (time sub (big, big));
  printf "%.2e    mul (small, no overflow)\n%!"
         (time mul (Z.of_int 42, Z.of_int 74));
  printf "%.2e    mul (small, overflow)\n%!"
         (time mul (Z.of_int max_int, Z.of_int 3));
  printf "%.2e    mul (small, big)\n%!"
         (time mul (Z.of_int 3, big));
  printf "%.2e    mul (medium, medium)\n%!"
         (time mul (med, med));
  printf "%.2e    mul (big, big)\n%!"
         (time mul (big, big));
  printf "%.2e    div (small, small)\n%!"
         (time div (Z.of_int 12345678, Z.of_int 443));
  printf "%.2e    div (big, small)\n%!"
         (time div (big, Z.of_int 443));
  printf "%.2e    div (big, medium)\n%!"
         (time div (big, med));
  printf "%.2e    shl (small, no overflow)\n%!"
         (time shl (Z.of_int 3, 10));
  printf "%.2e    shl (small, overflow)\n%!"
         (time shl (Z.of_int max_int, 2));
  printf "%.2e    shl (big)\n%!"
         (time shl (big, 42))
(* Factorial *)

let rec fact_z n =
  if n <= 0 then Z.one else Z.mul (Z.of_int n) (fact_z (n-1))

let _ =
  printf "%.2e    fact 10\n%!"
         (time_repeat 1_000_000 fact_z 10);
  printf "%.2e    fact 40\n%!"
         (time_repeat 10_000 fact_z 40);
  printf "%.2e    fact 200\n%!"
         (time_repeat 10_000 fact_z 200)

(* Fibonacci *)

let rec fib_int n =
  if n < 2 then 1 else fib_int(n-1) + fib_int(n-2)

let rec fib_natint n =
  if n < 2 then 1n else Nativeint.add (fib_natint(n-1)) (fib_natint(n-2))

let rec fib_z n =
  if n < 2 then Z.one else Z.add (fib_z(n-1)) (fib_z(n-2))

let fib_arg = 32

let _ =
  printf "%.2e    fib (int)\n%!"
         (time_repeat 100 fib_int fib_arg);
  printf "%.2e    fib (nativeint)\n%!"
         (time_repeat 100 fib_natint fib_arg);
  printf "%.2e    fib (Z)\n%!"
         (time_repeat 100 fib_z fib_arg)

(* Takeushi *)

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

let _ =
  printf "%.2e    tak (int)\n%!"
         (time_repeat 1000 tak_int (18,12,6));
  printf "%.2e    tak (nativeint)\n%!"
         (time_repeat 1000 tak_natint (18n,12n,6n));
  printf "%.2e    tak (Z)\n%!"
         (time_repeat 1000 tak_z (Z.of_int 18, Z.of_int 12, Z.of_int 6))

(* Pi digits computed with the streaming algorithm given on pages 4, 6
   & 7 of "Unbounded Spigot Algorithms for the Digits of Pi", Jeremy
   Gibbons, August 2004. *)

open Printf

let zero = Z.zero
and one = Z.one
and three = Z.of_int 3
and four = Z.of_int 4
and ten = Z.of_int 10
and neg_ten = Z.of_int (-10)
;;

(* Linear Fractional (aka M=F6bius) Transformations *)
module LFT = struct

  let floor_ev (q, r, s, t) x =
    Z.((q * x + r) / (s * x + t))

  let unit = (one, zero, zero, one)

  let comp (q, r, s, t) (q', r', s', t') =
    Z.(q * q' + r * s', q * r' + r * t',
       s * q' + t * s', s * r' + t * t')

end

let next z = LFT.floor_ev z three

let safe z n = (n = LFT.floor_ev z four)

let prod z n = LFT.comp (ten, Z.(neg_ten * n), zero, one) z

let cons z k =
  let den = 2 * k + 1 in
  LFT.comp z (Z.of_int k, Z.of_int (2 * den), zero, Z.of_int den)

let rec digit k z n row col =
  if n > 0 then
    let y = next z in
    if safe z y then
      if col = 10 then (
        let row = row + 10 in
        printf "\t:%i\n%a" row Z.output y;
        digit k (prod z y) (n - 1) row 1
      )
      else (
        printf "%a" Z.output y;
        digit k (prod z y) (n - 1) row (col + 1)
      )
    else digit (k + 1) (cons z k) n row col
  else
    printf "%*s\t:%i\n" (10 - col) "" (row + col)

let digits n = digit 1 LFT.unit n 0 0

let usage () =
  prerr_endline "Usage: pi <number of digits to compute for pi>";
  exit 2

let _ =
  let args = Sys.argv in
  if Array.length args <> 2 then usage () else
  digits (int_of_string Sys.argv.(1))

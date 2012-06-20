open Core.Std

module Code = struct
  type t = int * int * int with sexp

  let remove_digit n =
    let d = n mod 3 in
    ((n - d)/3, d)

  let of_num n =
    let (n,d1) = remove_digit n in
    let (n,d2) = remove_digit n in
    let (n,d3) = remove_digit n in
    (d3,d2,d1)

  let to_num (d3,d2,d1) =
    d1 + d2 * 3 + d3 * 9

  let of_letter l =
    let num =
      if l = ' ' then 0
      else 1 + Char.to_int l - Char.to_int 'a'
    in
    of_num num

  let to_letter t =
    let num = to_num t in
    if num = 0 then ' '
    else Char.of_int_exn (Char.to_int 'a' + num - 1)
end

let p i =
  let c = Code.of_num i in
  printf "'%c': %s\n"
    (Code.to_letter c) (Sexp.to_string (Code.sexp_of_t c))

let () =
  for c = 0 to 26 do p c done

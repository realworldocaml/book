type t = int * int

let all =
  [ 4, 02
  ; 4, 03
  ; 4, 04
  ; 4, 05
  ; 4, 06
  ; 4, 07
  ; 4, 08
  ; 4, 09
  ; 4, 10
  ; 4, 11
  ; 4, 12
  ]

let to_string (a, b) = Printf.sprintf "%d.%02d" a b
let to_int (a, b) = a * 100 + b

let of_string s =
  let t = Scanf.sscanf s "%u.%u" (fun a b -> (a, b)) in
  if List.mem t all then
    Some t
  else
    None

type t = A of t | B of (string * t) | C
let rec find pred =
  function
  |A t -> find pred t
  |B (x,t) -> if pred x then x else find pred t
  |C -> assert false

let v = A (A (A (A (B ("foo", A (A (B ("bar",C))))))))

let () =
  let mypred = function |"bar" -> true |_ -> false in
  ignore(find mypred v)

type t = | Alice | Bob

let test v =
  match v with
  | Alice   -> 100
  | Bob     -> 101

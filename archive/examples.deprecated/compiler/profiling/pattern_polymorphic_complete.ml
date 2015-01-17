type t = [ `Alice | `Bob | `Charlie | `David ]

let test (v:t) =
  match v with
  | `Alice   -> 100
  | `Bob     -> 101
  | `Charlie -> 102
  | `David   -> 103
